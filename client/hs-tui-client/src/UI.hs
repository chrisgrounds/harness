{-# LANGUAGE OverloadedStrings #-}

module UI (runUI) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Edit
import Client
import Control.Concurrent (Chan, forkIO, killThread, newChan, threadDelay, writeChan)
import Control.Exception (bracket)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Core
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import Network.Socket (Socket)
import System.Environment (lookupEnv)
import Text.Printf (printf)

data Name = Transcript | Composer deriving (Eq, Ord, Show)

data UI = UI
  { conversation :: Chat,
    composer :: Editor Text Name,
    waiting :: Bool,
    failure :: Maybe Text,
    frame :: Int,
    followLatest :: Bool,
    expandedTools :: Bool,
    helpVisible :: Bool,
    inputHistory :: [Text],
    historyIndex :: Maybe Int,
    savedDraft :: Text
  }

runUI :: FilePath -> Socket -> IO ()
runUI path sock = do
  events <- newBChan 16
  requests <- newChan
  noColor <- lookupEnv "NO_COLOR"
  let initial = UI (Chat [] Mercury3 []) (editorText Composer Nothing "") False Nothing 0 True False False [] Nothing ""
      publish = writeBChan events
      ticker = forever (threadDelay 120000 >> publish Tick)
      buildVty = mkVty V.defaultConfig
  bracket (forkIO (worker sock requests publish)) killThread $ \_ ->
    bracket (forkIO ticker) killThread $ \_ -> do
      vty <- buildVty
      void $ customMain vty buildVty (Just events) (app path requests (noColor /= Nothing)) initial

app :: FilePath -> Chan Message -> Bool -> App UI NetworkEvent Name
app path requests plain =
  App
    { appDraw = drawUI path,
      appChooseCursor = \s -> if helpVisible s then neverShowCursor s else showFirstCursor s,
      appHandleEvent = handleEvent requests,
      appStartEvent = pure (),
      appAttrMap = const (theme plain)
    }

handleEvent :: Chan Message -> BrickEvent Name NetworkEvent -> EventM Name UI ()
handleEvent requests event = do
  state <- get
  case event of
    AppEvent Tick -> when (waiting state) $ modify' (\s -> s {frame = (frame s + 1) `mod` 80})
    AppEvent (Received chat) -> do
      modify' (\s -> s {conversation = chat, waiting = False})
      when (followLatest state) scrollEnd
    AppEvent (Disconnected err) -> modify' (\s -> s {failure = Just err, waiting = False})
    VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
    VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) | T.null (draft state) -> halt
    VtyEvent (V.EvKey (V.KFun 1) []) -> modify' (\s -> s {helpVisible = not (helpVisible s)})
    VtyEvent (V.EvKey V.KEsc []) ->
      if helpVisible state then modify' (\s -> s {helpVisible = False}) else halt
    _ | helpVisible state -> pure ()
    VtyEvent (V.EvKey V.KPageUp []) -> scrollBack (vScrollPage transcript Up)
    VtyEvent (V.EvKey V.KPageDown []) -> scrollBack (vScrollPage transcript Down)
    VtyEvent (V.EvKey V.KHome [V.MCtrl]) -> scrollBack (vScrollToBeginning transcript)
    VtyEvent (V.EvKey V.KEnd [V.MCtrl]) -> do
      modify' (\s -> s {followLatest = True})
      scrollEnd
    VtyEvent (V.EvKey (V.KChar 't') [V.MCtrl]) -> do
      modify' (\s -> s {expandedTools = not (expandedTools s)})
      when (followLatest state) scrollEnd
    VtyEvent (V.EvKey V.KUp [V.MMeta]) -> recall True
    VtyEvent (V.EvKey V.KDown [V.MMeta]) -> recall False
    VtyEvent (V.EvKey V.KEnter []) -> submit requests
    VtyEvent (V.EvKey V.KEnter [V.MMeta]) -> edit (VtyEvent (V.EvKey V.KEnter []))
    VtyEvent (V.EvResize _ _) -> when (followLatest state) scrollEnd
    _ -> edit event
  where
    transcript = viewportScroll Transcript
    scrollBack :: EventM Name UI () -> EventM Name UI ()
    scrollBack action = modify' (\s -> s {followLatest = False}) >> action

scrollEnd :: EventM Name UI ()
scrollEnd = vScrollToEnd (viewportScroll Transcript)

draft :: UI -> Text
draft = T.intercalate "\n" . getEditContents . composer

edit :: BrickEvent Name NetworkEvent -> EventM Name UI ()
edit event = do
  current <- gets composer
  (updated, ()) <- nestEventM current (handleEditorEvent event)
  modify' (\s -> s {composer = updated, historyIndex = Nothing})

submit :: Chan Message -> EventM Name UI ()
submit requests = do
  state <- get
  let value = draft state
  if T.strip value == "/quit"
    then halt
    else when (not (waiting state) && failure state == Nothing && not (T.null (T.strip value))) $ do
      let message = userMessage value
      liftIO $ writeChan requests message
      modify'
        ( \s ->
            s
              { conversation = addMessage message (conversation s),
                composer = editorText Composer Nothing "",
                waiting = True,
                frame = 0,
                followLatest = True,
                inputHistory = value : inputHistory s,
                historyIndex = Nothing,
                savedDraft = ""
              }
        )
      scrollEnd

recall :: Bool -> EventM Name UI ()
recall older = do
  state <- get
  let history = inputHistory state
      index =
        if older
          then min (length history - 1) (maybe 0 (+ 1) (historyIndex state))
          else maybe (-1) (subtract 1) (historyIndex state)
  when (not (null history)) $ do
    let saved = if historyIndex state == Nothing then draft state else savedDraft state
        value = if index < 0 then saved else history !! index
    modify'
      ( \s ->
          s
            { composer = editorText Composer Nothing value,
              historyIndex = if index < 0 then Nothing else Just index,
              savedDraft = saved
            }
      )

drawUI :: FilePath -> UI -> [Widget Name]
drawUI path state = (if helpVisible state then [helpPanel] else []) ++ [screen]
  where
    chat = conversation state
    screen =
      withBorderStyle unicodeRounded $
        padAll 1 $
          hCenter $
            hLimit 104 $
              vBox
                [ padBottom (Pad 1) $
                    hBox
                      [ withAttr accent (txt "●  H A R N E S S"),
                        padLeft Max (withAttr muted (txt (modelLabel (model chat) <> "  ·  local session")))
                      ],
                  withAttr borderAttr $
                    borderWithLabel (txt " Conversation ") $
                      if null (messages chat)
                        then
                          center $
                            vBox
                              [ hCenter $ withAttr accent (txt "A little space to think."),
                                padTop (Pad 1) $ hCenter $ withAttr muted (txt "Ask a question, explore a file, or work through an idea."),
                                hCenter $ withAttr muted (txt "Your conversation and tool activity will appear here.")
                              ]
                        else
                          viewport Transcript Vertical $
                            padAll 1 $
                              vBox
                                (map (drawMessage state) (reverse (messages chat))),
                  padTop (Pad 1) $
                    hBox
                      [ status state,
                        padLeft Max $
                          withAttr muted $
                            txt
                              ( T.pack (show (length (messages chat)))
                                  <> " messages  ·  "
                                  <> T.pack (show (length (tools chat)))
                                  <> " tools"
                                  <> if followLatest state then "" else "  ·  scrollback"
                              )
                      ],
                  padTop (Pad 1) $
                    withAttr borderAttr $
                      borderWithLabel (txt " Message ") $
                        padLeftRight 1 $
                          vLimit (min 5 (length (getEditContents (composer state)))) $
                            renderEditor (txt . T.intercalate "\n") True (composer state),
                  padTop (Pad 1) $ withAttr muted $ txt "Enter send   Alt-Enter newline   PgUp/PgDn scroll   Ctrl-T tools   F1 help",
                  withAttr muted $ txt ("AF_UNIX  " <> safeText (T.pack path))
                ]

status :: UI -> Widget Name
status state = case failure state of
  Just err -> withAttr danger (txtWrap ("Disconnected · " <> safeText err <> "  ·  Ctrl-C to exit"))
  Nothing
    | waiting state -> withAttr accent $ txt (spinner <> "  Working" <> T.replicate (frame state `div` 8 `mod` 4) ".")
    | otherwise -> withAttr muted (txt "●  Ready")
  where
    spinner = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"] !! (frame state `mod` 10)

drawMessage :: UI -> Message -> Widget Name
drawMessage state message = padBottom (Pad 1) $ vBox (header : body ++ calls)
  where
    (label, color) = case role message of
      User -> ("YOU", userAttr)
      Assistant -> ("HARNESS", accent)
      System -> ("SYSTEM", muted)
      Tool -> (toolLabel, toolAttr)
    toolLabel = fromMaybe "TOOL RESULT" $ do
      callId <- toolCallId message
      call <- listToMaybe [c | m <- messages (conversation state), c <- toolCalls m, toolRequestId c == callId]
      pure ("↳ " <> T.pack (show (toolRequestFunctionParamsName (toolRequestFunction call))))
    header =
      hBox
        [ withAttr color (txt label),
          padLeft Max $ withAttr muted $ txt $ maybe "" (T.pack . printf "%.1f tokens/s") (tokensPerSecond message)
        ]
    body = case content message of
      Just value
        | not (T.null value) ->
            [ padTop (Pad 1) $
                padLeft (Pad 2) $
                  withDefAttr (if role message == Tool then muted else bodyAttr) $
                    if role message == Tool then toolOutput state value else txtWrap (safeText value)
            ]
      _ -> []
    calls =
      [ padTop (Pad 1) $
          padLeft (Pad 2) $
            withAttr toolAttr $
              vBox
                [ txt ("↳ " <> T.pack (show (toolRequestFunctionParamsName function))),
                  withAttr muted $ txtWrap (safeText (toolRequestFunctionParamsArguments function))
                ]
      | call <- toolCalls message,
        let function = toolRequestFunction call
      ]

toolOutput :: UI -> Text -> Widget Name
toolOutput state value
  | expandedTools state || length rows <= 5 = txtWrap (safeText value)
  | otherwise =
      vBox
        [ txtWrap (T.unlines (take 5 rows)),
          withAttr toolAttr $ txt ("… " <> T.pack (show (length rows - 5)) <> " more lines · Ctrl-T to expand")
        ]
  where
    rows = T.lines (safeText value)

helpPanel :: Widget Name
helpPanel =
  center $
    hLimit 68 $
      withBorderStyle unicodeRounded $
        borderWithLabel (txt " Keyboard shortcuts ") $
          padAll 2 $
            vBox
              [ txt "Enter          Send a message",
                txt "Alt-Enter      Insert a newline",
                txt "Alt-Up/Down    Recall sent messages (restores your draft)",
                txt "PgUp/PgDn      Scroll conversation",
                txt "Ctrl-Home/End  First message / follow latest",
                txt "Ctrl-T         Expand or collapse tool results",
                txt "Ctrl-C / Esc   Exit (Esc closes this help first)",
                txt "Ctrl-D         Exit with an empty composer",
                padTop (Pad 1) hBorder,
                padTop (Pad 1) $ withAttr muted $ txt "F1 or Esc to close · Compose while Harness works."
              ]

modelLabel :: Model -> Text
modelLabel Mercury3 = "mercury-3"

accent, muted, borderAttr, userAttr, toolAttr, bodyAttr, danger :: AttrName
accent = attrName "accent"
muted = attrName "muted"
borderAttr = attrName "border"
userAttr = attrName "user"
toolAttr = attrName "tool"
bodyAttr = attrName "body"
danger = attrName "danger"

theme :: Bool -> AttrMap
theme plain
  | plain = attrMap V.defAttr []
  | otherwise =
      attrMap
        (V.withForeColor V.defAttr (V.rgbColor (218 :: Int) 224 235))
        [ (accent, V.withStyle (fg (V.rgbColor (112 :: Int) 211 208)) V.bold),
          (muted, fg (V.rgbColor (139 :: Int) 151 172)),
          (borderAttr, fg (V.rgbColor (72 :: Int) 85 108)),
          (userAttr, V.withStyle (fg (V.rgbColor (173 :: Int) 185 237)) V.bold),
          (toolAttr, fg (V.rgbColor (221 :: Int) 184 116)),
          (bodyAttr, fg (V.rgbColor (218 :: Int) 224 235)),
          (danger, fg (V.rgbColor (242 :: Int) 139 139)),
          (editAttr, fg (V.rgbColor (218 :: Int) 224 235)),
          (editFocusedAttr, fg (V.rgbColor (218 :: Int) 224 235))
        ]
