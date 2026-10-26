{-# LANGUAGE OverloadedStrings #-}

module UI (runUI) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
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
import qualified Data.Aeson as Aeson
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Config as VC
import Graphics.Vty.CrossPlatform (mkVty)
import Network.Socket (Socket)
import TerminalWidth (installTerminalWidths)
import Theme (loadTheme, accent, muted, metadataAttr, borderAttr, userAttr, toolAttr, bodyAttr, danger, successAttr)
import Text.Printf (printf)

data Name = Transcript | Composer deriving (Eq, Ord, Show)

data UI = UI
  { conversation :: Chat,
    composer :: Editor Text Name,
    waiting :: Bool,
    failure :: Maybe Text,
    frame :: Int,
    followLatest :: Bool,
    inputHistory :: [Text],
    historyIndex :: Maybe Int,
    savedDraft :: Text
  }

runUI :: FilePath -> Socket -> IO ()
runUI path sock = do
  events <- newBChan 16
  requests <- newChan
  let initial = UI (Chat [] Mercury3 []) (editorText Composer Nothing "") False Nothing 0 True [] Nothing ""
      publish = writeBChan events
      ticker = forever (threadDelay 120000 >> publish Tick)
      buildVty = do
        let config =
              VC.defaultConfig
                { VC.configInputMap =
                    [ (Nothing, inputBytes, V.EvKey V.KEnter [V.MShift])
                    | inputBytes <- ["\ESC[13;2u", "\ESC[27;2;13~"]
                    ]
                }
        vty <- mkVty config
        installTerminalWidths
        let output = V.outputIface vty
        mapM_
          (\mode -> when (V.supportsMode output mode) $ V.setMode output mode True)
          [V.BracketedPaste, V.Mouse]
        pure vty
  attrs <- loadTheme
  bracket (forkIO (worker sock requests publish)) killThread $ \_ ->
    bracket (forkIO ticker) killThread $ \_ -> do
      vty <- buildVty
      void $ customMain vty buildVty (Just events) (app path requests attrs) initial

app :: FilePath -> Chan Message -> AttrMap -> App UI NetworkEvent Name
app path requests attrs =
  App
    { appDraw = drawUI path,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent requests,
      appStartEvent = pure (),
      appAttrMap = const attrs
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
    VtyEvent (V.EvKey V.KEsc []) -> pure ()
    MouseDown Transcript V.BScrollUp _ _ -> scrollBack (vScrollBy transcript (-3))
    MouseDown Transcript V.BScrollDown _ _ -> scrollBack (vScrollBy transcript 3)
    VtyEvent (V.EvKey V.KPageUp []) -> scrollBack (vScrollPage transcript Up)
    VtyEvent (V.EvKey V.KPageDown []) -> scrollBack (vScrollPage transcript Down)
    VtyEvent (V.EvKey V.KHome [V.MCtrl]) -> scrollBack (vScrollToBeginning transcript)
    VtyEvent (V.EvKey V.KEnd [V.MCtrl]) -> do
      modify' (\s -> s {followLatest = True})
      scrollEnd
    VtyEvent (V.EvKey V.KUp [V.MMeta]) -> recall True
    VtyEvent (V.EvKey V.KDown [V.MMeta]) -> recall False
    VtyEvent (V.EvKey V.KEnter []) -> submit requests
    VtyEvent (V.EvKey V.KEnter [V.MShift]) -> edit (VtyEvent (V.EvKey V.KEnter []))
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
drawUI path state = [screen]
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
                      [ withAttr accent (txt "Harness"),
                        padLeft Max $
                          withAttr metadataAttr $
                            txt ((case Aeson.toJSON (model chat) of
                                   Aeson.String s -> s
                                   _ -> error "modelLabel: expected a JSON string encoding") <> "  ·  local session")
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
                          withVScrollBars OnRight $
                            clickable Transcript $
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
                  padTop (Pad 1) $ withAttr muted $ txt "Enter send   Shift-Enter newline   Scroll/PgUp/PgDn history   Ctrl-C exit",
                  withAttr muted $ txt ("AF_UNIX  " <> safeText (T.pack path))
                ]

status :: UI -> Widget Name
status state = case failure state of
  Just err -> withAttr danger (txtWrap ("Disconnected · " <> safeText err <> "  ·  Ctrl-C to exit"))
  Nothing
    | waiting state -> withAttr accent $ txt (spinner <> "  Working" <> T.replicate (frame state `div` 8 `mod` 4) ".")
    | otherwise -> withAttr successAttr (txt "●  Ready")
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
          padLeft Max $ withAttr metadataAttr $ txt $ maybe "" (T.pack . printf "%.1f tokens/s") (tokensPerSecond message)
        ]
    body = case content message of
      Just value
        | not (T.null value) ->
            [ padTop (Pad 1) $
                padLeft (Pad 2) $
                  withAttr (if role message == Tool then muted else bodyAttr) $
                    txtWrap (safeText value)
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
