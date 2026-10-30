{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module UI (runUI) where

import Client (ClientTask, NetworkEvent, safeText, worker)
import Client.Tokens (averageTokens)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TQueue, atomically, flushTQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (bracket, bracket_, finally)
import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.|.))
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import DearImGui hiding (end, flags, indent, size, w, x, y, z)
import qualified DearImGui.Raw as Raw
import DearImGui.SDL (pollEventsWithImGui, sdl2NewFrame, sdl2Shutdown)
import DearImGui.SDL.Renderer (sdl2InitForSDLRenderer, sdlRendererInit, sdlRendererNewFrame, sdlRendererRenderDrawData, sdlRendererShutdown)
import Foreign (alloca, freeHaskellFunPtr, nullPtr, peek, with)
import ImGui.Extras (configureIO, ensureLayout, keyPressed, takeKey)
import Network.Socket (Socket)
import qualified SDL
import qualified SDL.Internal.Types as SDL.Internal
import SDL.Raw.Enum (pattern SDL_USEREVENT, pattern SDL_WINDOWEVENT_EXPOSED, pattern SDL_WINDOWEVENT_SIZE_CHANGED)
import qualified SDL.Raw.Event as SDL.Raw
import qualified SDL.Raw.Types as SDL.Raw
import qualified SDL.Raw.Video as SDL.Raw
import System.Directory (getHomeDirectory)
import Theme
import Theme.Palette
import UI.Appearance
import qualified UI.Composer as Composer
import UI.Fonts
import UI.Layout
import UI.Motion
import qualified UI.Settings as Settings
import UI.Sidebar
import UI.State
import UI.Tools
import qualified UI.Transcript as Transcript
import UI.Widgets

-- | Application resources. Feature components own their rendering state;
-- only the window layout and shared visual resources live at this level.
data Env = Env
  { appearance :: Appearance,
    composerState :: Composer.ComposerState,
    transcript :: Transcript.Transcript,
    socketPath :: FilePath,
    projectDir :: FilePath,
    projectShown :: Text,
    motion :: Motion,
    sidebarSize :: IORef Float
  }

-- | Everything a frame needs.
data Loop = Loop
  { env :: Env,
    renderer :: SDL.Renderer,
    window :: SDL.Window,
    inbox :: TQueue NetworkEvent,
    requests :: TQueue ClientTask,
    state :: IORef UI,
    -- | Set while a frame is drawn, so a resize cannot start another.
    drawing :: IORef Bool
  }

runUI :: FilePath -> FilePath -> Socket -> IO ()
runUI path dir sock = do
  colours <- loadPalette
  home <- getHomeDirectory
  newsQueue <- newTQueueIO
  requestQueue <- newTQueueIO
  bracket_ (SDL.initialize [SDL.InitVideo]) SDL.quit $
    bracket (SDL.createWindow appName windowConfig) SDL.destroyWindow $ \win -> do
      SDL.windowMinimumSize win SDL.$= SDL.V2 560 460
      bracket (SDL.createRenderer win (-1) rendererConfig) SDL.destroyRenderer $ \ren ->
        bracket createContext destroyContext $ \_ ->
          bracket_ (sdl2InitForSDLRenderer win ren) sdl2Shutdown $
            bracket_ (sdlRendererInit ren) sdlRendererShutdown $ do
              ensureLayout
              (faces, scale) <- loadFonts =<< pixelDensity win ren
              configureIO scale
              applyTheme colours
              let look = Appearance colours faces
              moves <- newMotion
              box <- Composer.newComposerState look moves
              transcriptState <- Transcript.newTranscript look moves
              sideSize <- newIORef 276
              ui <- newIORef initialUI
              busy <- newIORef False
              let shown = if home `isPrefixOf` dir then "~" <> drop (length home) dir else dir
                  env' = Env look box transcriptState path dir (T.pack shown) moves sideSize
                  loop = Loop env' ren win newsQueue requestQueue ui busy
                  publish event = atomically (writeTQueue newsQueue event) >> wake
              bracket (forkIO (worker sock (atomically (readTQueue requestQueue)) publish)) killThread $ \_ ->
                whileResizing loop $ frames loop settleFrames
  where
    windowConfig =
      SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 1180 860,
          SDL.windowResizable = True,
          SDL.windowHighDPI = True,
          SDL.windowPosition = SDL.Centered
        }
    rendererConfig = SDL.defaultRenderer {SDL.rendererType = SDL.AcceleratedVSyncRenderer}

appName :: Text
appName = "Holey, Algebraic Agents"

-- | Pixels per point: 2 on a Retina display.
pixelDensity :: SDL.Window -> SDL.Renderer -> IO Float
pixelDensity win (SDL.Internal.Renderer ren) = do
  SDL.V2 points _ <- SDL.get (SDL.windowSize win)
  pixels <- alloca $ \pw -> alloca $ \ph -> SDL.Raw.getRendererOutputSize ren pw ph >> peek pw
  pure (if points > 0 && pixels > 0 then fromIntegral pixels / fromIntegral points else 1)

-- | Wakes the frame loop if it is waiting for input. Safe from any thread.
wake :: IO ()
wake = void $ with (SDL.Raw.UserEvent SDL_USEREVENT 0 0 0 nullPtr nullPtr) SDL.Raw.pushEvent

-- | Runs the action, drawing frames while the window is resized: macOS
-- holds the event loop until the user lets go, which would otherwise
-- freeze the window's contents.
whileResizing :: Loop -> IO a -> IO a
whileResizing loop action = do
  watch <- SDL.Raw.mkEventFilter $ \_ event -> do
    payload <- peek event
    case payload of
      SDL.Raw.WindowEvent {SDL.Raw.windowEventEvent = kind}
        | kind == SDL_WINDOWEVENT_SIZE_CHANGED || kind == SDL_WINDOWEVENT_EXPOSED -> void (frame loop)
      _ -> pure ()
    pure 1
  bracket_ (SDL.Raw.addEventWatch watch nullPtr) (SDL.Raw.delEventWatch watch nullPtr >> freeHaskellFunPtr watch) action

-- | Frames drawn after the last input before waiting for more, since Dear
-- ImGui settles hovering and layout over a frame or two.
settleFrames :: Int
settleFrames = 3

-- | Draws frames until the window closes or the user quits: one for every
-- input or server event, and one every display refresh while a run is in
-- progress or anything is still moving.
frames :: Loop -> Int -> IO ()
frames loop settle = do
  ui <- readIORef (state loop)
  animating <- moving (motion (env loop))
  when (not (waiting ui) && not animating && settle <= 0) $ void (SDL.Raw.waitEventTimeout nullPtr 500)
  events <- pollEventsWithImGui
  changed <- frame loop
  next <- readIORef (state loop)
  let closed = any ((== SDL.QuitEvent) . SDL.eventPayload) events
  unless (closed || quitting next) $ frames loop (if null events && not changed then settle - 1 else settleFrames)

-- | Draws one frame and applies what happened in it. Returns whether
-- anything did.
frame :: Loop -> IO Bool
frame loop = do
  busy <- readIORef (drawing loop)
  if busy
    then pure False
    else bracket_ (writeIORef (drawing loop) True) (writeIORef (drawing loop) False) $ do
      let e = env loop
          dir = projectDir e
      news <- atomically (flushTQueue (inbox loop))
      current <- fst . updateAll dir (map Network news) <$> readIORef (state loop)
      sdlRendererNewFrame
      sdl2NewFrame
      newFrame
      startFrame (motion e) =<< Raw.getTime
      actions <- draw e current
      render
      -- Dear ImGui lays out in points; the renderer draws in pixels.
      density <- realToFrac <$> pixelDensity (window loop) (renderer loop)
      SDL.rendererScale (renderer loop) SDL.$= SDL.V2 density density
      let Colour r g b = background (palette (appearance e))
      SDL.rendererDrawColor (renderer loop) SDL.$= SDL.V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
      SDL.clear (renderer loop)
      sdlRendererRenderDrawData (renderer loop) =<< getDrawData
      SDL.present (renderer loop)
      let (next, started) = updateAll dir actions current
      writeIORef (state loop) next
      mapM_ (atomically . writeTQueue (requests loop)) started
      pure (not (null news && null actions))

draw :: Env -> UI -> IO [Action]
draw e ui = do
  (shown, size') <- fullscreen $ do
    keys <- if settingsOpen ui then pure [] else shortcuts (currentChatId ui)
    ImVec2 width height <- Raw.getContentRegionAvail
    rememberedWidth <- readIORef (sidebarSize e)
    let withSidebar = width >= 860
        side = if withSidebar then max 180 (min (min 460 (width - 380)) rememberedWidth) else 0
        gutter = if withSidebar then 8 else 0
    picked <-
      if withSidebar
        then do
          let view = SidebarView
                { sidebarTitle = appName,
                  sidebarProject = projectShown e,
                  sidebarSocket = socketPath e,
                  sidebarChats = chatSummaries ui,
                  sidebarCurrent = currentChatId ui,
                  sidebarRate = averageTokens (tokenStats ui)
                }
          picked <- sidebar (appearance e) view (ImVec2 side height)
          setCursorPos' (ImVec2 side 0)
          nextWidth <- withStyleVec ImGuiStyleVar_WindowPadding (ImVec2 0 0) $
            pane "sidebar-divider" (ImVec2 gutter height) Nothing False $
              resizeSidebar side 180 (min 460 (width - 380)) height
          writeIORef (sidebarSize e) nextWidth
          divider <- colour 1 (seam (palette (appearance e)))
          line divider 1 (ImVec2 (side + gutter / 2) 0) (ImVec2 (side + gutter / 2) height)
          pure picked
        else pure []
    setCursorPos' (ImVec2 (side + gutter) 0)
    typed <- mainPane e ui (not withSidebar) (ImVec2 (width - side - gutter) height)
    let settingsView = Settings.SettingsView
          { Settings.settingsVisible = settingsOpen ui,
            Settings.settingsSnapshot = openAISettings ui,
            Settings.settingsBusy = settingsBusy ui,
            Settings.settingsError = settingsError ui
          }
    settings <- Settings.settingsModal (appearance e) settingsView (ImVec2 width height)
    pure (keys ++ picked ++ typed ++ settings)
  closed <- if showingTools ui then toolsWindow (appearance e) size' else pure []
  pure (shown ++ map (Chat (currentChatId ui)) closed)

-- | The window filling the display, behind any other, and its size.
fullscreen :: IO [Action] -> IO ([Action], ImVec2)
fullscreen contents = do
  Raw.setNextWindowFullscreen
  visible <- withStyleVec ImGuiStyleVar_WindowPadding (ImVec2 0 0) $ withText appName $ \name -> Raw.begin name Nothing (Just windowFlags)
  flip finally Raw.end $ do
    display <- Raw.getWindowSize
    actions <- if visible then contents else pure []
    pure (actions, display)
  where
    windowFlags =
      foldr
        (.|.)
        ImGuiWindowFlags_NoDecoration
        [ImGuiWindowFlags_NoMove, ImGuiWindowFlags_NoResize, ImGuiWindowFlags_NoSavedSettings, ImGuiWindowFlags_NoBringToFrontOnFocus, ImGuiWindowFlags_NoScrollWithMouse]

-- | Keys that work wherever the keyboard is.
shortcuts :: Int -> IO [Action]
shortcuts chat = do
  pressed <- mapM (\(action, test) -> (,) action <$> test) bindings
  pure [action | (action, True) <- pressed]
  where
    bindings =
      [ -- Taken from the composer too, which would otherwise undo its edits.
        (Chat chat (CloseTools), takeKey [] ImGuiKey_Escape),
        (Chat chat (Recall Older), keyPressed [ImGuiMod_Alt] ImGuiKey_UpArrow),
        (Chat chat (Recall Newer), keyPressed [ImGuiMod_Alt] ImGuiKey_DownArrow),
        (Chat chat (ScrollTo PageUp), keyPressed [] ImGuiKey_PageUp),
        (Chat chat (ScrollTo PageDown), keyPressed [] ImGuiKey_PageDown),
        (Chat chat (ScrollTo ToFirst), keyPressed [ImGuiMod_Ctrl] ImGuiKey_Home),
        (Chat chat (ScrollTo ToLatest), keyPressed [ImGuiMod_Ctrl] ImGuiKey_End),
        (OpenSettings, takeKey [ImGuiMod_Ctrl] ImGuiKey_Comma),
        (NewChat, takeKey [ImGuiMod_Ctrl] ImGuiKey_N),
        (Exit, keyPressed [ImGuiMod_Ctrl] ImGuiKey_Q)
      ]

-- | The conversation, the status line and the composer, in a column.
mainPane :: Env -> UI -> Bool -> ImVec2 -> IO [Action]
mainPane e ui titled size'@(ImVec2 width _) =
  pane "main" size' Nothing False $ do
    let column = min 720 (width - 2 * 44)
        left = (width - column) / 2
    forM_ (failure ui) $ \err -> do
      gap 18
      setCursorPosX left
      _ <- icon Cross (danger p)
      paragraph (measure left column) (danger p) ("Lost the connection to the server: " <> safeText err <> ". Start it again with cabal run server, then reopen this window.")
    when titled $ do
      gap 18
      setCursorPosX left
      withFont (heading (fonts (appearance e))) $ label (foreground p) appName
    forM_ (runError ui) $ \err -> do
      gap 12
      setCursorPosX left
      paragraph (measure left column) (danger p) (safeText err)
    navigation <- if titled then do
      setCursorPosX left
      new <- navigationButton NewChatButton
      sameLine
      settings <- navigationButton SettingsButton
      pure ([NewChat | new] ++ [OpenSettings | settings])
      else pure []
    let composerView = Composer.ComposerView
          { Composer.composerDraft = draft ui,
            Composer.composerReplacement = replacement ui,
            Composer.composerCanSend = canSend ui,
            Composer.composerFailure = failure ui,
            Composer.composerBlocked = settingsOpen ui,
            Composer.composerWaiting = waiting ui,
            Composer.composerChoices = modelChoices ui,
            Composer.composerChoice = selectedChoice ui,
            Composer.composerEffort = selectedEffort ui
          }
    layout <- Composer.measureComposer (composerState e) composerView (status ui) column
    scrolled <- pane "transcript" (ImVec2 width (negate (Composer.reservedHeight layout))) Nothing True $
      Transcript.drawTranscript (transcript e)
        (Transcript.TranscriptView (currentChatId ui) (runs ui) (scrollRequest ui)) left column
    composed <- Composer.drawComposer (composerState e) composerView layout (status ui) left column
    pure (navigation ++ map (Chat (currentChatId ui)) (scrolled ++ composed))
  where
    p = palette (appearance e)
