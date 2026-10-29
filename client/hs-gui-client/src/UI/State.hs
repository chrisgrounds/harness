{-# LANGUAGE OverloadedStrings #-}

-- | The interface's state and how it changes, with no drawing: the window
-- turns what the user does into 'Action's and draws the state that results.
module UI.State
  ( UI (..),
    Scroll (..),
    Direction (..),
    Action (..),
    initialUI,
    update,
    updateAll,
    canSend,
    Status (..),
    status,
    implementCounts,
    target,
    rateText,
    runSummary,
    toolCallLabel,
    allToolNames,
  )
where

import Client (NetworkEvent (..), safeText)
import Client.Commands (Command (..), parseCommand)
import Client.Tokens (TokenStats, addRate, emptyTokenStats)
import Core
import Data.Maybe (isNothing, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

data UI = UI
  { -- | Newest first; the head is the run for the latest requirement.
    runs :: [Run],
    -- | The composer's text.
    draft :: Text,
    -- | Text the composer is to show in place of its own, with the cursor
    -- at the end: set when sending clears the draft or recall replaces it,
    -- and cleared once the composer shows it.
    replacement :: Maybe Text,
    waiting :: Bool,
    failure :: Maybe Text,
    -- | Sent messages, newest first.
    inputHistory :: [Text],
    -- | Which sent message the composer shows, while recalling.
    historyIndex :: Maybe Int,
    -- | The draft from before recalling, restored after the newest.
    savedDraft :: Text,
    -- | The model the server runs with, once its 'Connected' greeting arrives.
    serverModel :: Maybe Model,
    -- | Token-rate statistics across every message received from the server.
    tokenStats :: TokenStats,
    -- | Whether the tools window is open: '/tools' opens it, and Esc, its
    -- close button or the next requirement closes it.
    showingTools :: Bool,
    -- | A scroll the transcript is to make on the next frame.
    scrollRequest :: Maybe Scroll,
    quitting :: Bool
  }
  deriving (Eq, Show)

data Scroll
  = ToFirst
  | ToLatest
  | PageUp
  | PageDown
  | -- | The run at this position, oldest first.
    ToRun Int
  deriving (Eq, Show)

data Direction = Older | Newer
  deriving (Eq, Show)

data Action
  = Network NetworkEvent
  | -- | The user changed the composer's text.
    Edited Text
  | -- | The composer shows the 'replacement'.
    Replaced
  | Send
  | -- | Show an older or newer sent message in the composer.
    Recall Direction
  | CloseTools
  | ScrollTo Scroll
  | -- | The transcript made the requested scroll.
    Scrolled
  | Exit
  deriving (Eq, Show)

initialUI :: UI
initialUI =
  UI
    { runs = [],
      draft = "",
      replacement = Nothing,
      waiting = False,
      failure = Nothing,
      inputHistory = [],
      historyIndex = Nothing,
      savedDraft = "",
      serverModel = Nothing,
      tokenStats = emptyTokenStats,
      showingTools = False,
      scrollRequest = Nothing,
      quitting = False
    }

-- | Applies an action, with the run to start in the given project, if it
-- starts one.
update :: FilePath -> Action -> UI -> (UI, Maybe StartRun)
update dir action ui = case action of
  Network event -> (receive event ui, Nothing)
  -- Until the composer shows a replacement, its text is the one replaced.
  Edited value | isNothing (replacement ui) -> (ui {draft = value, historyIndex = Nothing}, Nothing)
  Edited _ -> (ui, Nothing)
  Replaced -> (ui {replacement = Nothing}, Nothing)
  Send -> send dir ui
  Recall direction -> (recall direction ui, Nothing)
  CloseTools -> (ui {showingTools = False}, Nothing)
  ScrollTo scroll -> (ui {scrollRequest = Just scroll}, Nothing)
  Scrolled -> (ui {scrollRequest = Nothing}, Nothing)
  Exit -> (ui {quitting = True}, Nothing)

-- | Applies actions in order, with the runs they start.
updateAll :: FilePath -> [Action] -> UI -> (UI, [StartRun])
updateAll dir actions ui = foldl apply (ui, []) actions
  where
    apply (current, started) action =
      let (next, run) = update dir action current in (next, started ++ maybeToList run)

receive :: NetworkEvent -> UI -> UI
receive event ui = case event of
  RunChanged run -> ui {runs = run : drop 1 (runs ui)}
  TurnDone -> ui {waiting = False}
  Disconnected err -> ui {failure = Just err, waiting = False}
  ServerModel m -> ui {serverModel = Just m}
  ReplyRate r -> ui {tokenStats = addRate (tokenStats ui) r}

-- | Whether sending the draft would start a run: the server is connected
-- and idle, and the draft is not blank.
canSend :: UI -> Bool
canSend ui = not (waiting ui) && isNothing (failure ui) && not (T.null (T.strip (draft ui)))

send :: FilePath -> UI -> (UI, Maybe StartRun)
send dir ui = case parseCommand (draft ui) of
  Quit -> (ui {quitting = True}, Nothing)
  ListTools -> (sent {showingTools = True}, Nothing)
  RunRequirement req
    | canSend ui ->
        ( sent
            { runs = newRun req : runs ui,
              waiting = True,
              showingTools = False,
              scrollRequest = Just ToLatest
            },
          Just (StartRun dir req)
        )
    | otherwise -> (ui, Nothing)
  where
    sent = (setDraft "" ui) {inputHistory = draft ui : inputHistory ui, historyIndex = Nothing, savedDraft = ""}

recall :: Direction -> UI -> UI
recall direction ui
  | null sent = ui
  | otherwise = (setDraft value ui) {historyIndex = if index < 0 then Nothing else Just index, savedDraft = saved}
  where
    sent = inputHistory ui
    index = case direction of
      Older -> min (length sent - 1) (maybe 0 (+ 1) (historyIndex ui))
      Newer -> maybe (-1) (subtract 1) (historyIndex ui)
    saved = if isNothing (historyIndex ui) then draft ui else savedDraft ui
    value = if index < 0 then saved else sent !! index

setDraft :: Text -> UI -> UI
setDraft value ui = ui {draft = value, replacement = Just value}

-- | What the status line shows.
data Status
  = -- | The connection is lost, and why.
    Offline Text
  | -- | A run is in progress: the step, with its progress, and the model's
    -- latest tool call.
    Busy Text (Maybe Text)
  | -- | The latest run stopped at a step that failed.
    Stopped Step
  | Ready
  deriving (Eq, Show)

status :: UI -> Status
status ui = case failure ui of
  Just err -> Offline (safeText err)
  Nothing -> case (waiting ui, latest) of
    (True, Just (step, _)) -> Busy (stepLabel step <> progress step) (toolCallLabel <$> (run >>= latestToolCall))
    (True, Nothing) -> Busy "Starting" Nothing
    (False, Just (step, Failed _)) -> Stopped step
    _ -> Ready
  where
    run = listToMaybe (runs ui)
    latest = run >>= currentStep
    progress step = maybe "" (\(done, total) -> "  " <> T.pack (show done) <> "/" <> T.pack (show total)) (run >>= implementCounts step)

-- | Finished and total functions, while the step is implementing them.
implementCounts :: Step -> Run -> Maybe (Int, Int)
implementCounts Implement run | (done, total) <- taskProgress run, total > 0 = Just (done, total)
implementCounts _ _ = Nothing

-- | The run's main module and its component, then how many others it changes.
target :: Run -> Maybe Text
target run = do
  plan <- mainModule (artefact run)
  let others = length (modules (artefact run)) - 1
  pure $
    moduleNameText (planModule plan)
      <> " in "
      <> componentNameText (planComponent plan)
      <> (if others > 0 then "  +" <> T.pack (show others) <> " more" else "")

-- | The average token rate of the model's replies, e.g. @25 tokens a second@.
rateText :: Double -> Text
rateText r = T.pack (printf "%.0f" r) <> " tokens a second"

-- | How a run is doing, for the run list: what it is working on, or how
-- it ended.
runSummary :: Run -> Text
runSummary run = case currentStep run of
  Just (step, Failed _) -> "Stopped at " <> T.toLower (stepLabel step)
  Just (step, _) -> stepLabel step <> maybe "" (\(done, total) -> " " <> T.pack (show done) <> "/" <> T.pack (show total)) (implementCounts step run)
  Nothing
    | all ((== Passed) . snd) (steps run) -> "Done"
    | otherwise -> "Waiting"

-- | A tool call as the status line shows it: the tool's name and what it was
-- called on, shortened to fit.
toolCallLabel :: ToolCall -> Text
toolCallLabel c = toolNameText (callTool c) <> maybe "" ((" " <>) . shorten . safeText) (callSubject c)
  where
    shorten s
      | T.length s > 60 = T.take 59 s <> "…"
      | otherwise = s

-- | The name of every tool the harness can call, in declaration order.
allToolNames :: [Text]
allToolNames = map toolNameText [minBound .. maxBound]
