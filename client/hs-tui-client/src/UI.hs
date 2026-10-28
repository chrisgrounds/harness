{-# LANGUAGE OverloadedStrings #-}

module UI (runUI, runsSummary) where

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
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Config as VC
import Graphics.Vty.CrossPlatform (mkVty)
import Network.Socket (Socket)
import TerminalWidth (installTerminalWidths)
import Theme (loadTheme, accent, muted, metadataAttr, borderAttr, userAttr, bodyAttr, danger, successAttr)

data Name = Transcript | Composer deriving (Eq, Ord, Show)

data UI = UI
  { -- | Newest first; the head is the run for the latest requirement.
    runs :: [Run],
    composer :: Editor Text Name,
    waiting :: Bool,
    failure :: Maybe Text,
    frame :: Int,
    followLatest :: Bool,
    inputHistory :: [Text],
    historyIndex :: Maybe Int,
    savedDraft :: Text,
    -- | The model the server runs with, once its 'Connected' greeting arrives.
    serverModel :: Maybe Model
  }

runUI :: FilePath -> FilePath -> Socket -> IO ()
runUI path dir sock = do
  events <- newBChan 16
  requests <- newChan
  let initial = UI [] (editorText Composer Nothing "") False Nothing 0 True [] Nothing "" Nothing
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
      void $ customMain vty buildVty (Just events) (app path dir requests attrs) initial

app :: FilePath -> FilePath -> Chan StartRun -> AttrMap -> App UI NetworkEvent Name
app path dir requests attrs =
  App
    { appDraw = drawUI path dir,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent dir requests,
      appStartEvent = pure (),
      appAttrMap = const attrs
    }

handleEvent :: FilePath -> Chan StartRun -> BrickEvent Name NetworkEvent -> EventM Name UI ()
handleEvent dir requests event = do
  state <- get
  case event of
    AppEvent Tick -> when (waiting state) $ modify' (\s -> s {frame = (frame s + 1) `mod` 80})
    AppEvent (RunChanged run) -> do
      modify' (\s -> s {runs = run : drop 1 (runs s)})
      when (followLatest state) scrollEnd
    AppEvent TurnDone -> modify' (\s -> s {waiting = False})
    AppEvent (Disconnected err) -> modify' (\s -> s {failure = Just err, waiting = False})
    AppEvent (ServerModel m) -> modify' (\s -> s {serverModel = Just m})
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
    VtyEvent (V.EvKey V.KEnter []) -> submit dir requests
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

submit :: FilePath -> Chan StartRun -> EventM Name UI ()
submit dir requests = do
  state <- get
  let value = draft state
  if T.strip value == "/quit"
    then halt
    else when (not (waiting state) && failure state == Nothing && not (T.null (T.strip value))) $ do
      let req = Requirement value
      liftIO $ writeChan requests (StartRun dir req)
      modify'
        ( \s ->
            s
              { runs = newRun req : runs s,
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
  let sent = inputHistory state
      index =
        if older
          then min (length sent - 1) (maybe 0 (+ 1) (historyIndex state))
          else maybe (-1) (subtract 1) (historyIndex state)
  when (not (null sent)) $ do
    let saved = if historyIndex state == Nothing then draft state else savedDraft state
        value = if index < 0 then saved else sent !! index
    modify'
      ( \s ->
          s
            { composer = editorText Composer Nothing value,
              historyIndex = if index < 0 then Nothing else Just index,
              savedDraft = saved
            }
      )

drawUI :: FilePath -> FilePath -> UI -> [Widget Name]
drawUI path dir state = [screen]
  where
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
                            txt ("holey  ·  " <> safeText (T.pack dir))
                      ],
                  withAttr borderAttr $
                    borderWithLabel (txt " Conversation ") $
                      if null (runs state)
                        then
                          center $
                            vBox
                              [ hCenter $ withAttr accent (txt "Describe what to build."),
                                padTop (Pad 1) $ hCenter $ withAttr muted (txt "Each message starts a run: types, functions, properties,"),
                                hCenter $ withAttr muted (txt "then parallel implementation, type checking and property tests.")
                              ]
                        else
                          withVScrollBars OnRight $
                            clickable Transcript $
                              viewport Transcript Vertical $
                                padAll 1 $
                                  vBox
                                    (map (drawRun state) (reverse (runs state))),
                  padTop (Pad 1) $
                    hBox
                      [ status state,
                        padLeft Max $
                          withAttr muted $
                            txt (safeText (runsSummary (serverModel state) (length (runs state)) (followLatest state)))
                      ],
                  padTop (Pad 1) $
                    withAttr borderAttr $
                      borderWithLabel (txt " Message ") $
                        padLeftRight 1 $
                          drawComposer (composer state),
                  padTop (Pad 1) $ withAttr muted $ txt "Enter send   Shift-Enter newline   Scroll/PgUp/PgDn history   Ctrl-C exit",
                  withAttr muted $ txt ("AF_UNIX  " <> safeText (T.pack path))
                ]

-- | The editor soft-wrapped to the full available width, growing from
-- 'minRows' to 'maxRows' and then scrolling to keep the cursor in view. Brick's 'renderEditor'
-- scrolls sideways instead, so the cursor is placed here.
drawComposer :: Editor Text Name -> Widget Name
drawComposer box = Widget Greedy Fixed $ do
  width <- max 1 . availWidth <$> getContext
  let (row, col) = getCursorPosition box
      contents = getEditContents box
      wrapped = [wrapLine width (i == row) line | (i, line) <- zip [0 ..] contents]
      before = sum (map length (take row wrapped))
      (cursorRow, cursorCol) = case drop row contents of
        line : _ -> cursorIn width (T.take col line)
        [] -> (0, 0)
      rows = concat wrapped
      top = max 0 (before + cursorRow - (maxRows - 1))
      shown = take maxRows (drop top rows)
  render $
    showCursor Composer (Location (cursorCol, before + cursorRow - top)) $
      padRight Max (vBox (map drawRow (take (max minRows (length shown)) (shown ++ repeat ""))))
  where
    -- 'txt' draws nothing for empty text, which would collapse the box.
    drawRow r = txt (if T.null r then " " else r)
    minRows = 1
    maxRows = 5

-- | Splits a line into rows of at most @width@ columns. The cursor's line
-- gets an empty row after it when full, so a cursor at its end has room.
wrapLine :: Int -> Bool -> Text -> [Text]
wrapLine width hasCursor line
  | T.null line = [""]
  | otherwise =
      let rows = go line
       in if hasCursor && textWidth (last rows) >= width then rows ++ [""] else rows
  where
    go t
      | T.null t = []
      | otherwise = let (row, rest) = splitAtWidth width t in row : go rest

-- | Where the cursor lands when @prefix@, the text before it, is wrapped.
cursorIn :: Int -> Text -> (Int, Int)
cursorIn width = go 0
  where
    go r t =
      let (row, rest) = splitAtWidth width t
       in if T.null rest && textWidth row < width then (r, textWidth row) else go (r + 1) rest

-- | The longest prefix that fits in @width@ columns, taking at least one
-- character so wide characters in a narrow box still make progress.
splitAtWidth :: Int -> Text -> (Text, Text)
splitAtWidth width t = T.splitAt (max 1 (fits 0 0 (T.unpack t))) t
  where
    fits n _ [] = n
    fits n used (c : cs)
      | used + w > width = n
      | otherwise = fits (n + 1) (used + w) cs
      where
        w = textWidth (T.singleton c)


status :: UI -> Widget Name
status state = case failure state of
  Just err -> withAttr danger (txtWrap ("Disconnected · " <> safeText err <> "  ·  Ctrl-C to exit"))
  Nothing -> case (waiting state, latest) of
    (True, Just (step, _)) -> withAttr accent $ txt (spinner state <> "  " <> stepLabel step <> progress step <> iterationNote)
    (True, Nothing) -> withAttr accent $ txt (spinner state <> "  Starting" <> T.replicate (frame state `div` 8 `mod` 4) ".")
    (False, Just (step, Failed _)) -> withAttr danger $ txt ("✗  Stopped at " <> stepLabel step)
    _ -> withAttr successAttr (txt "●  Ready")
  where
    run = listToMaybe (runs state)
    latest = run >>= currentStep
    progress step = maybe "" (implementProgress step) run
    iterationNote = case run of
      Just r | iteration r > 1 -> "  ·  iteration " <> T.pack (show (iteration r))
      _ -> ""

spinner :: UI -> Text
spinner state = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"] !! (frame state `mod` 10)

implementProgress :: Step -> Run -> Text
implementProgress Implement run | (done, total) <- taskProgress run, total > 0 = "  " <> T.pack (show done) <> "/" <> T.pack (show total)
implementProgress _ _ = ""

drawRun :: UI -> Run -> Widget Name
drawRun state run =
  padBottom (Pad 1) $
    vBox $
      [ withAttr userAttr (txt "YOU"),
        padTop (Pad 1) $ padLeft (Pad 2) $ withAttr bodyAttr $ txtWrap (safeText (requirementText (requirement run))),
        padTop (Pad 1) $
          hBox
            [ withAttr accent (txt "HOLEY"),
              withAttr metadataAttr $ txt (maybe "" ("  →  " <>) (target run)),
              padLeft Max $ withAttr metadataAttr $ txt ("iteration " <> T.pack (show (iteration run)))
            ],
        padTop (Pad 1) $ padLeft (Pad 2) $ vBox (concatMap (drawStep state run) (steps run))
      ]
        ++ [ padTop (Pad 1) $ padLeft (Pad 2) $ vBox [withAttr muted (txtWrap ("↺  " <> safeText note)) | note <- history run]
           | not (null (history run))
           ]
        ++ [ padTop (Pad 1) $ padLeft (Pad 2) $ withAttr danger $ txtWrap ("!  " <> safeText warning)
           | warning <- warnings run
           ]
        ++ [ padTop (Pad 1) $
               padLeft (Pad 2) $
                 vBox (withAttr muted (txt "Files") : [withAttr metadataAttr (txt ("   " <> safeText (T.pack file))) | file <- touchedFiles run])
           | not (null (touchedFiles run))
           ]

-- | The run's main module and its component, then how many others it changes.
target :: Run -> Maybe Text
target run = do
  plan <- mainModule (artefact run)
  let others = length (modules (artefact run)) - 1
  pure $
    moduleNameText (planModule plan)
      <> " in "
      <> planComponent plan
      <> (if others > 0 then "  +" <> T.pack (show others) <> " more" else "")

drawStep :: UI -> Run -> (Step, StepStatus) -> [Widget Name]
drawStep state run (step, stepStatus) = row : details
  where
    row = withAttr (stepAttr stepStatus) $ txt (icon <> "  " <> stepLabel step <> implementProgress step run)
    icon = if stepStatus == Running then spinner state else statusIcon stepStatus
    details = case stepStatus of
      Failed reason -> [padLeft (Pad 3) $ withAttr danger $ txtWrap (safeText reason)]
      Running | step == Implement -> map (padLeft (Pad 3) . drawTask state) (tasks run)
      _ -> []

drawTask :: UI -> (FunctionRef, TaskStatus) -> Widget Name
drawTask state (ref, taskStatus) = case taskStatus of
  Queued -> withAttr muted $ txt ("○  " <> label)
  Working -> withAttr accent $ txt (spinner state <> "  " <> label)
  Done -> withAttr successAttr $ txt ("✓  " <> label)
  TaskFailed reason -> withAttr danger $ txtWrap ("✗  " <> label <> ": " <> safeText reason)
  where
    label = safeText (functionNameText (refName ref) <> "  " <> T.pack (refPath ref))

stepAttr :: StepStatus -> AttrName
stepAttr Pending = muted
stepAttr Running = accent
stepAttr Passed = successAttr
stepAttr (Failed _) = danger

-- | The text at the right of the status line: the server's model if known,
-- the number of runs, and whether the transcript is scrolled back, e.g.
-- @claude-opus-5-5  ·  3 runs  ·  scrollback@.
runsSummary :: Maybe Model -> Int -> Bool -> Text
runsSummary known count following =
  maybe "" (\m -> modelId m <> "  ·  ") known
    <> T.pack (show count)
    <> (if count == 1 then " run" else " runs")
    <> (if following then "" else "  ·  scrollback")
