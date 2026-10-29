{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module UI (runUI) where

import Client (NetworkEvent, safeText, worker)
import Client.Tokens (averageTokens)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TQueue, atomically, flushTQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (bracket, bracket_, finally)
import Control.Monad (forM, forM_, unless, void, when, zipWithM)
import Core
import Data.Bits ((.|.))
import Data.IORef
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import DearImGui hiding (end, flags, indent, size, w, x, y, z)
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.DrawList as DrawList
import DearImGui.SDL(pollEventsWithImGui, sdl2NewFrame, sdl2Shutdown)
import DearImGui.SDL.Renderer (sdl2InitForSDLRenderer, sdlRendererInit, sdlRendererNewFrame, sdlRendererRenderDrawData, sdlRendererShutdown)
import Foreign (alloca, freeHaskellFunPtr, nullPtr, peek, with)
import Foreign.C.Types (CBool)
import ImGui.Extras
import Network.Socket (Socket)
import qualified SDL
import qualified SDL.Internal.Types as SDL.Internal
import SDL.Raw.Enum (pattern SDL_USEREVENT, pattern SDL_WINDOWEVENT_EXPOSED, pattern SDL_WINDOWEVENT_SIZE_CHANGED)
import qualified SDL.Raw.Event as SDL.Raw
import qualified SDL.Raw.Types as SDL.Raw
import qualified SDL.Raw.Video as SDL.Raw
import System.Directory (getHomeDirectory)
import System.FilePath (dropTrailingPathSeparator, takeFileName)
import Theme
import Theme.Palette
import UI.Fonts
import UI.Motion
import UI.State
import UI.Widgets

data Env = Env
  { palette :: Palette,
    fonts :: Fonts,
    composerText :: Composer,
    socketPath :: FilePath,
    projectDir :: FilePath,
    -- | The project's directory with the home directory as @~@.
    projectShown :: Text,
    motion :: Motion,
    scrolling :: IORef Scrolling,
    -- | Where each run starts in the transcript, oldest first, as of the
    -- last frame.
    runTops :: IORef (Map.Map Int Float),
    -- | Whether the composer had the keyboard last frame.
    composing :: IORef Bool
  }

-- | How the transcript is scrolling.
data Scrolling = Scrolling
  { -- | Kept at the bottom as output arrives.
    following :: Bool,
    -- | Where a requested scroll is gliding to.
    destination :: Maybe Float,
    -- | The position set last frame, to tell when the user scrolled.
    lastSet :: Maybe Float
  }

-- | Everything a frame needs.
data Loop = Loop
  { env :: Env,
    renderer :: SDL.Renderer,
    window :: SDL.Window,
    inbox :: TQueue NetworkEvent,
    requests :: TQueue StartRun,
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
    bracket (SDL.createWindow (T.pack (projectName dir)) windowConfig) SDL.destroyWindow $ \win -> do
      SDL.windowMinimumSize win SDL.$= SDL.V2 560 460
      bracket (SDL.createRenderer win (-1) rendererConfig) SDL.destroyRenderer $ \ren ->
        bracket createContext destroyContext $ \_ ->
          bracket_ (sdl2InitForSDLRenderer win ren) sdl2Shutdown $
            bracket_ (sdlRendererInit ren) sdlRendererShutdown $ do
              ensureLayout
              (faces, scale) <- loadFonts =<< pixelDensity win ren
              configureIO scale
              applyTheme colours
              box <- newComposer
              moves <- newMotion
              scroller <- newIORef (Scrolling True Nothing Nothing)
              tops <- newIORef Map.empty
              active <- newIORef False
              ui <- newIORef initialUI
              busy <- newIORef False
              let shown = if home `isPrefixOf` dir then "~" <> drop (length home) dir else dir
                  env' = Env colours faces box path dir (T.pack shown) moves scroller tops active
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

projectName :: FilePath -> FilePath
projectName = takeFileName . dropTrailingPathSeparator

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
      startFrame (motion e)
      actions <- draw e current
      render
      -- Dear ImGui lays out in points; the renderer draws in pixels.
      density <- realToFrac <$> pixelDensity (window loop) (renderer loop)
      SDL.rendererScale (renderer loop) SDL.$= SDL.V2 density density
      let Colour r g b = background (palette e)
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
    keys <- shortcuts
    ImVec2 width height <- Raw.getContentRegionAvail
    let withSidebar = width >= 860
        side = if withSidebar then sidebarWidth else 0
    picked <-
      if withSidebar
        then do
          picked <- sidebar e ui (ImVec2 side height)
          divider <- colour 1 (seam (palette e))
          line divider 1 (ImVec2 side 0) (ImVec2 side height)
          pure picked
        else pure []
    setCursorPos' (ImVec2 side 0)
    typed <- mainPane e ui (not withSidebar) (ImVec2 (width - side) height)
    pure (keys ++ picked ++ typed)
  closed <- if showingTools ui then toolsWindow e size' else pure []
  pure (shown ++ closed)

setCursorPos' :: ImVec2 -> IO ()
setCursorPos' (ImVec2 px py) = setCursorPosX px >> setCursorPosY py

sidebarWidth :: Float
sidebarWidth = 276

-- | The window filling the display, behind any other, and its size.
fullscreen :: IO [Action] -> IO ([Action], ImVec2)
fullscreen contents = do
  Raw.setNextWindowFullscreen
  visible <- withStyleVec ImGuiStyleVar_WindowPadding (ImVec2 0 0) $ withText "Harness" $ \name -> Raw.begin name Nothing (Just windowFlags)
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
shortcuts :: IO [Action]
shortcuts = do
  pressed <- mapM (\(action, test) -> (,) action <$> test) bindings
  pure [action | (action, True) <- pressed]
  where
    bindings =
      [ -- Taken from the composer too, which would otherwise undo its edits.
        (CloseTools, takeKey [] ImGuiKey_Escape),
        (Recall Older, keyPressed [ImGuiMod_Alt] ImGuiKey_UpArrow),
        (Recall Newer, keyPressed [ImGuiMod_Alt] ImGuiKey_DownArrow),
        (ScrollTo PageUp, keyPressed [] ImGuiKey_PageUp),
        (ScrollTo PageDown, keyPressed [] ImGuiKey_PageDown),
        (ScrollTo ToFirst, keyPressed [ImGuiMod_Ctrl] ImGuiKey_Home),
        (ScrollTo ToLatest, keyPressed [ImGuiMod_Ctrl] ImGuiKey_End),
        (Exit, keyPressed [ImGuiMod_Ctrl] ImGuiKey_Q)
      ]

-- | A child window of the given size and background, without scroll bars
-- unless asked for.
pane :: Text -> ImVec2 -> Maybe Colour -> Bool -> IO a -> IO a
pane name size' fill scrolls contents = do
  let flags' =
        (if isJust fill then ImGuiWindowFlags_None else ImGuiWindowFlags_NoBackground)
          .|. (if scrolls then ImGuiWindowFlags_None else ImGuiWindowFlags_NoScrollbar .|. ImGuiWindowFlags_NoScrollWithMouse)
      painted = maybe id (\c -> bracket_ (with (vec4 c) (Raw.pushStyleColor ImGuiCol_ChildBg)) (Raw.popStyleColor 1)) fill
  painted $ withStyleFloat ImGuiStyleVar_ChildRounding 0 $ withChild name size' False flags' (const contents)

-- | The project, every run with how it is doing, newest first, and the
-- model. Clicking a run scrolls the transcript to it.
sidebar :: Env -> UI -> ImVec2 -> IO [Action]
sidebar e ui size'@(ImVec2 width height) =
  pane "sidebar" size' (Just (raised p)) False $ do
    let inner = width - 2 * margin
    gap 26
    setCursorPosX margin
    withFont (heading (fonts e)) $ label (foreground p) =<< ellipsize inner (T.pack (projectName (projectDir e)))
    setCursorPosX margin
    withFont (small (fonts e)) $ label (muted p) =<< ellipsize inner (safeText (projectShown e))
    gap 22
    footerHeight <- (* 3) <$> withFont (small (fonts e)) getTextLineHeightWithSpacing
    ImVec2 _ listTop <- Raw.getCursorPos
    picked <-
      pane "runs" (ImVec2 width (height - listTop - footerHeight - 24)) Nothing True $
        concat <$> forM (reverse (zip [0 ..] (reverse (runs ui)))) (runRow e width)
    setCursorPosY (height - footerHeight - 16)
    withFont (small (fonts e)) $ do
      setCursorPosX margin
      let (signal, connection) = case failure ui of
            Just _ -> (danger p, "Disconnected")
            Nothing -> (success p, maybe "Connecting" modelId (serverModel ui))
      _ <- icon Dot signal
      label (foreground p) connection
      setCursorPosX margin
      label (muted p) (maybe "No replies yet" rateText (averageTokens (tokenStats ui)))
      setCursorPosX margin
      label (muted p) =<< ellipsize inner (safeText (T.pack (socketPath e)))
    pure picked
  where
    p = palette e
    margin = 22

-- | One run in the sidebar: what was asked and how it is doing.
runRow :: Env -> Float -> (Int, Run) -> IO [Action]
runRow e width (index, run) = do
  bodyHeight <- getTextLineHeight
  smallHeight <- withFont (small (fonts e)) getTextLineHeight
  let rowHeight = bodyHeight + smallHeight + 2 * padY + 2
      rowWidth = width - 2 * inset
  ImVec2 _ top <- Raw.getCursorPos
  setCursorPosX inset
  clicked <- with (ImVec2 rowWidth rowHeight) $ \s -> withText ("##run" <> T.pack (show index)) $ \n -> Raw.invisibleButton n s ImGuiButtonFlags_None
  hovered <- Raw.isItemHovered
  when hovered $ do
    handCursor
    list <- Raw.getWindowDrawList
    fill <- colour 1 (lifted p)
    corner <- Raw.getItemRectMin
    far <- Raw.getItemRectMax
    with corner $ \a -> with far $ \b -> DrawList.addRectFilled list a b fill 8 ImDrawFlags_None
  let (kind, tone) = runIcon p run
  setCursorPos' (ImVec2 (inset + padX) (top + padY))
  _ <- icon kind tone
  textLeft <- getCursorPosX
  label (foreground p) =<< ellipsize (width - inset - padX - textLeft) (safeText (requirementText (requirement run)))
  setCursorPos' (ImVec2 textLeft (top + padY + bodyHeight + 2))
  withFont (small (fonts e)) $ label (if waitingOn run then accent p else muted p) (runSummary run)
  setCursorPos' (ImVec2 0 (top + rowHeight + 2))
  pure [ScrollTo (ToRun index) | clicked]
  where
    p = palette e
    inset = 10
    padX = 12
    padY = 9
    waitingOn r = maybe False ((== Running) . snd) (currentStep r)

-- | How a run as a whole is doing.
runIcon :: Palette -> Run -> (Icon, Colour)
runIcon p run = case currentStep run of
  Just (_, Failed _) -> (Cross, danger p)
  Just _ -> (Spinner, accent p)
  Nothing
    | all ((== Passed) . snd) (steps run) -> (Tick, success p)
    | otherwise -> (Hollow, muted p)

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
      withFont (heading (fonts e)) $ label (foreground p) (T.pack (projectName (projectDir e)))
    below <- belowTranscript e ui
    scrolled <- pane "transcript" (ImVec2 width (negate below)) Nothing True $ do
      if null (runs ui)
        then emptyTranscript e left column
        else void (zipWithM (drawRun e left column) [0 ..] (reverse (runs ui)))
      gap 28
      follow e ui
    gap statusGap
    setCursorPosX left
    statusLine e ui left column
    gap composerGap
    composed <- composerRow e ui left column
    pure (scrolled ++ composed)
  where
    p = palette e

-- | Where prose in the column wraps: under 80 characters a line.
measure :: Float -> Float -> Float
measure left column = left + min column 600

-- | What an empty transcript asks.
emptyTranscript :: Env -> Float -> Float -> IO ()
emptyTranscript e left column = do
  ImVec2 _ avail <- Raw.getContentRegionAvail
  gap (max 40 (avail * 0.28))
  setCursorPosX left
  withFont (heading (fonts e)) $ paragraph (measure left column) (foreground p) "Describe what to build."
  gap 6
  setCursorPosX left
  paragraph (measure left column) (muted p) "Each message starts a run: types, functions, properties,\nthen parallel implementation, type checking and property tests."
  where
    p = palette e

-- | Glides to a requested scroll, or keeps the latest output in view while
-- the transcript is at the bottom. Scrolling by hand stops following, and
-- scrolling back to the bottom starts again.
follow :: Env -> UI -> IO [Action]
follow e ui = do
  y <- scrollY
  bottom <- scrollMaxY
  page <- getWindowHeight
  tops <- readIORef (runTops e)
  s <- readIORef (scrolling e)
  dt <- elapsed (motion e)
  let movedByHand = maybe False (\l -> abs (y - l) > 1) (lastSet s)
      from = fromMaybe y (destination s)
      requested = case scrollRequest ui of
        Just ToFirst -> Just (Just 0)
        Just ToLatest -> Just Nothing
        Just PageUp -> Just (Just (from - page * 0.85))
        Just PageDown -> Just (Just (from + page * 0.85))
        Just (ToRun i) -> Just (subtract 24 <$> Map.lookup i tops)
        Nothing -> Nothing
      (following', destination') = case requested of
        Just Nothing -> (True, Nothing)
        Just (Just d) -> (False, Just (clamp d))
        Nothing
          | movedByHand -> (y >= bottom - 1, Nothing)
          | otherwise -> (following s, destination s)
      clamp = max 0 . min bottom
      goal = if following' then Just bottom else destination'
  case goal of
    Just d -> do
      let next = d + (y - d) * exp (negate dt / 0.075)
          arrived = abs (next - d) < 0.5
          set = if arrived then d else next
      unless arrived (keepMoving (motion e))
      setScrollY set
      writeIORef (scrolling e) (Scrolling following' (if arrived then Nothing else destination') (Just set))
    Nothing -> writeIORef (scrolling e) (Scrolling following' Nothing Nothing)
  pure [Scrolled | isJust (scrollRequest ui)]

-- | A run: the requirement, then its steps on a rail that fills as they
-- pass, then why it looped back, any warnings and the files it changed.
drawRun :: Env -> Float -> Float -> Int -> Run -> IO ()
drawRun e left column index run = do
  ImVec2 _ top <- Raw.getCursorPos
  modifyIORef' (runTops e) (Map.insert index top)
  -- A new run fades in as it is sent.
  appear <- easeFrom (motion e) (key "appear") 0.09 0 1
  withAlpha appear $ do
    when (index > 0) $ do
      gap 30
      rule <- colour 1 (seam p)
      ImVec2 _ ruleY <- Raw.getCursorScreenPos
      ImVec2 windowX _ <- Raw.getWindowPos
      line rule 1 (ImVec2 (windowX + left) ruleY) (ImVec2 (windowX + left + column) ruleY)
      gap 30
    when (index == 0) (gap 30)
    requirementBlock e left column (safeText (requirementText (requirement run)))
    gap 16
    setCursorPosX left
    forM_ (target run) $ \t -> withFont (mono (fonts e)) $ label (muted p) (safeText t)
    when (isJust (target run) && iteration run > 1) $ rightAt (left + column) (muted p) ("Iteration " <> T.pack (show (iteration run)))
    gap 10
    nodes <- forM (steps run) (drawStep e left column run)
    rail e run nodes
    unless (null (history run)) $ do
      gap 12
      forM_ (history run) $ \note -> do
        setCursorPosX left
        -- Loop-backs name their iteration; the review's conclusion does not.
        _ <- icon (if "Iteration" `T.isPrefixOf` note then Retry else Dot) (muted p)
        paragraph (measure left column) (muted p) (safeText note)
    forM_ (warnings run) $ \warning -> do
      gap 10
      setCursorPosX left
      _ <- icon Caution (danger p)
      paragraph (measure left column) (danger p) (safeText warning)
    unless (null (touchedFiles run)) $ do
      gap 14
      withFont (mono (fonts e)) $
        forM_ (touchedFiles run) $ \file -> do
          setCursorPosX left
          label (if changedKind file == NewFile then success p else secondaryAccent p) (safeText (changeTag file))
  where
    p = palette e
    key name = name <> T.pack (show index)

-- | The user's words, set a weight up beside a bar in their colour.
requirementBlock :: Env -> Float -> Float -> Text -> IO ()
requirementBlock e left column words' = do
  setCursorPosX (left + 16)
  ImVec2 x0 y0 <- Raw.getCursorScreenPos
  withFont (lead (fonts e)) $ paragraph (measure left column) (foreground p) words'
  ImVec2 _ below <- Raw.getCursorScreenPos
  bar <- colour 1 (highlight p)
  line bar 3 (ImVec2 (x0 - 16) (y0 + 3)) (ImVec2 (x0 - 16) (below - snd (itemSpacing metrics) - 3))
  where
    p = palette e

-- | Text in a colour ending at the given position on the current line.
rightAt :: Float -> Colour -> Text -> IO ()
rightAt edge c t = do
  width <- textWidth t
  Raw.sameLine
  setCursorPosX (edge - width)
  label c t

-- | A step on the rail, with its failure or, while functions are being
-- implemented, their progress. Returns its icon's centre and radius, and
-- whether it passed.
drawStep :: Env -> Float -> Float -> Run -> (Step, StepStatus) -> IO ((ImVec2, Float), Bool)
drawStep e left column run (step, stepStatus) = do
  let (kind, tone) = statusIcon p stepStatus
      counts = implementCounts step run
  setCursorPosX left
  node <- icon kind tone
  label (if stepStatus == Passed then foreground p else tone) (stepLabel step <> maybe "" (\(done, total) -> "  " <> T.pack (show done) <> "/" <> T.pack (show total)) counts)
  indent' <- (left +) <$> iconWidth
  case stepStatus of
    -- Usually the compiler's own words, so set as code.
    Failed reason -> do
      gap 2
      setCursorPosX indent'
      withFont (mono (fonts e)) $ paragraph (left + column) (danger p) (safeText reason)
      gap 4
    Running | step == Implement -> do
      forM_ counts $ \(done, total) -> do
        gap 2
        setCursorPosX indent'
        progress <- ease (motion e) ("progress" <> T.pack (show (iteration run)) <> requirementText (requirement run)) 0.12 (fromIntegral done / fromIntegral total)
        meter (seam p) (accent p) (min 300 (column - (indent' - left))) progress
        gap 4
      mapM_ (drawTask e indent' (left + column)) (tasks run)
      gap 4
    _ -> pure ()
  -- Room between steps, so the rail reads as one line through them.
  gap 2
  pure (node, stepStatus == Passed)
  where
    p = palette e

-- | The line joining a run's steps: grey, filling with the passed colour
-- down from each step that passes, and draining as a new iteration resets
-- them.
rail :: Env -> Run -> [((ImVec2, Float), Bool)] -> IO ()
rail e run nodes = do
  track <- colour 1 (seam p)
  forM_ (zip3 [0 :: Int ..] nodes (drop 1 nodes)) $ \(k, ((ImVec2 cx y0, r0), passed), ((ImVec2 _ y1, r1), _)) -> do
    let top = y0 + r0 + 3
        bottom = y1 - r1 - 3
    when (bottom > top) $ do
      line track 1.5 (ImVec2 cx top) (ImVec2 cx bottom)
      filled <- ease (motion e) ("rail" <> T.pack (show k) <> requirementText (requirement run)) 0.14 (if passed then 1 else 0)
      when (filled > 0.01) $ do
        fill <- colour (0.75 * filled) (success p)
        line fill 1.5 (ImVec2 cx top) (ImVec2 cx (top + (bottom - top) * filled))
  where
    p = palette e

drawTask :: Env -> Float -> Float -> (FunctionRef, TaskStatus) -> IO ()
drawTask e indent' edge (ref, taskStatus) = do
  let (kind, tone) = taskIcon p taskStatus
  setCursorPosX indent'
  _ <- icon kind tone
  withFont (mono (fonts e)) $ label (if taskStatus == Queued then muted p else foreground p) (safeText (functionNameText (refName ref)))
  Raw.sameLine
  setCursorPosY . (+ 1) =<< getCursorPosY
  withFont (small (fonts e)) $ label (muted p) (safeText (T.pack (refPath ref)))
  case taskStatus of
    TaskFailed reason -> do
      nested <- (indent' +) <$> iconWidth
      setCursorPosX nested
      paragraph edge (danger p) (safeText reason)
    _ -> pure ()
  where
    p = palette e

statusIcon :: Palette -> StepStatus -> (Icon, Colour)
statusIcon p Pending = (Hollow, muted p)
statusIcon p Running = (Spinner, accent p)
statusIcon p Passed = (Tick, success p)
statusIcon p (Failed _) = (Cross, danger p)

taskIcon :: Palette -> TaskStatus -> (Icon, Colour)
taskIcon p Queued = (Hollow, muted p)
taskIcon p Working = (Spinner, accent p)
taskIcon p Done = (Tick, success p)
taskIcon p (TaskFailed _) = (Cross, danger p)

-- | What the latest run is doing, above the composer.
statusLine :: Env -> UI -> Float -> Float -> IO ()
statusLine e ui left column = do
  case status ui of
    Offline _ -> icon Cross (danger p) >> label (danger p) "Disconnected"
    Busy doing detail -> do
      _ <- icon Spinner (accent p)
      label (accent p) doing
      forM_ detail $ \call -> do
        Raw.sameLine
        here <- getCursorPosX
        bodyHeight <- getTextLineHeight
        withFont (small (fonts e)) $ do
          shown <- ellipsize (left + column - here - 8) (safeText call)
          -- Lines the smaller text up with the body's baseline.
          smallHeight <- getTextLineHeight
          setCursorPosY . (+ max 0 ((bodyHeight - smallHeight) * 0.8)) =<< getCursorPosY
          label (muted p) shown
    Stopped step -> icon Cross (danger p) >> label (danger p) ("Stopped at " <> T.toLower (stepLabel step))
    Ready -> icon Dot (success p) >> label (muted p) "Ready"
  where
    p = palette e

-- | The composer and its send button.
composerRow :: Env -> UI -> Float -> Float -> IO [Action]
composerRow e ui left column = do
  height <- composerHeight e ui
  wasActive <- readIORef (composing e)
  focusGlow <- ease (motion e) "focus" 0.1 (if wasActive then 1 else 0)
  -- The edge lightens while typing; orange is kept for what is running.
  let edge = mix (realToFrac (0.22 * focusGlow)) (foreground p) (seam p)
      fieldWidth = column - sendDiameter - 12
  setCursorPosX left
  ImVec2 _ top <- Raw.getCursorPos
  -- Keep the keyboard in the composer unless something else is in use.
  focus <- not <$> Raw.isAnyItemActive
  (submitted, replaced, value) <-
    withStyleVec ImGuiStyleVar_FramePadding (ImVec2 16 (snd (framePadding metrics))) $
      withStyleFloat ImGuiStyleVar_FrameRounding 14 $
        bracket_ (with (vec4 edge) (Raw.pushStyleColor ImGuiCol_Border)) (Raw.popStyleColor 1) $
          composer (composerText e) hint (ImVec2 fieldWidth height) focus (replacement ui)
  writeIORef (composing e) =<< Raw.isItemActive
  setCursorPos' (ImVec2 (left + fieldWidth + 12) (top + (height - sendDiameter) / 2))
  clicked <- sendButton sendDiameter (canSend ui) (accent p) (secondaryAccent p) (seam p) (foreground p)
  pure ([Replaced | replaced] ++ [Edited value | value /= draft ui] ++ [Send | submitted || clicked])
  where
    p = palette e
    hint = case failure ui of
      Just _ -> "Disconnected from the server"
      Nothing -> "Enter sends, Shift-Enter adds a line, /tools lists the tools"

sendDiameter :: Float
sendDiameter = 42

-- | Two to six lines, as many as the draft has, growing smoothly.
composerHeight :: Env -> UI -> IO Float
composerHeight e ui = do
  lineHeight <- getTextLineHeight
  let rows = max 2 (min 6 (length (T.splitOn "\n" (draft ui))))
  ease (motion e) "composer" 0.07 (fromIntegral rows * lineHeight + 2 * snd (framePadding metrics))

statusGap, composerGap, bottomGap :: Float
statusGap = 4
composerGap = 6
bottomGap = 20

-- | The height everything under the transcript takes, as 'mainPane' lays
-- it out: each item and the spacing after it.
belowTranscript :: Env -> UI -> IO Float
belowTranscript e ui = do
  lineHeight <- getTextLineHeight
  composer' <- composerHeight e ui
  let items = [statusGap, lineHeight, composerGap, composer']
  pure (sum items + snd (itemSpacing metrics) * fromIntegral (length items) + bottomGap)

-- | The window '/tools' opens, centred on the display: every tool the
-- model can call.
toolsWindow :: Env -> ImVec2 -> IO [Action]
toolsWindow e (ImVec2 width height) = do
  with (ImVec2 (width / 2) (height / 2)) $ \centre ->
    with (ImVec2 0.5 0.5) $ \pivot -> Raw.setNextWindowPos centre ImGuiCond_Appearing (Just pivot)
  with (1 :: CBool) $ \open -> do
    visible <-
      withStyleVec ImGuiStyleVar_WindowPadding (ImVec2 22 18) $
        withText "Tools the model can call" $ \name -> Raw.begin name (Just open) (Just windowFlags)
    flip finally Raw.end $
      when visible $ withFont (mono (fonts e)) $ mapM_ (label (foreground p)) allToolNames
    stillOpen <- peek open
    pure [CloseTools | stillOpen == 0]
  where
    p = palette e
    windowFlags = ImGuiWindowFlags_AlwaysAutoResize .|. ImGuiWindowFlags_NoCollapse .|. ImGuiWindowFlags_NoSavedSettings
