{-# LANGUAGE OverloadedStrings #-}

-- | The interface's state and how it changes, with no drawing: the window
-- turns what the user does into 'Action's and draws the state that results.
module UI.State
  ( UI (..), Conversation, Scroll (..), Direction (..), Action (..),
    initialUI, update, updateAll, canSend, Status (..), status,
    implementCounts, target, rateText, runSummary, toolCallLabel, allToolNames,
    runs, draft, replacement, waiting, failure, runError, inputHistory, historyIndex,
    savedDraft, serverModel, tokenStats, showingTools, scrollRequest, quitting,
    chatSummaries, selectedChoice
  ) where

import Client (NetworkEvent (..), ClientTask (..), safeText)
import Client.Settings
import qualified Data.Map.Strict as Map
import Client.Commands (Command (..), parseCommand)
import Client.Tokens (TokenStats, addRate, emptyTokenStats)
import Core
import Data.Maybe (isNothing, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

data Conversation = Conversation
  { -- | Newest first; the head is the run for the latest requirement.
    cRuns :: [Run],
    -- | The composer's text.
    cDraft :: Text,
    -- | Text the composer is to show in place of its own, with the cursor
    -- at the end: set when sending clears the draft or recall replaces it,
    -- and cleared once the composer shows it.
    cReplacement :: Maybe Text,
    cWaiting :: Bool,
    cFailure :: Maybe Text,
    cRunError :: Maybe Text,
    -- | Sent messages, newest first.
    cInputHistory :: [Text],
    -- | Which sent message the composer shows, while recalling.
    cHistoryIndex :: Maybe Int,
    -- | The draft from before recalling, restored after the newest.
    cSavedDraft :: Text,
    -- | The model the server runs with, once its 'Connected' greeting arrives.
    cServerModel :: Maybe Model,
    -- | Token-rate statistics across every message received from the server.
    cTokenStats :: TokenStats,
    -- | Whether the tools window is open: '/tools' opens it, and Esc, its
    -- close button or the next requirement closes it.
    cShowingTools :: Bool,
    -- | A scroll the transcript is to make on the next frame.
    cScrollRequest :: Maybe Scroll,
    cQuitting :: Bool
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
  | -- | The composer shows the 'cReplacement'.
    Replaced
  | Send
  | -- | Show an older or newer sent message in the composer.
    Recall Direction
  | CloseTools
  | ScrollTo Scroll
  | -- | The transcript made the requested scroll.
    Scrolled
  | Exit
  | NewChat
  | SelectChat Int
  | ChooseModel ModelChoice
  | OpenSettings
  | CloseSettings
  | ConnectOpenAI Bool
  | DisconnectOpenAI
  | ChangeAccount Text
  | RefreshModels
  | CancelLogin
  deriving (Eq, Show)

initialConversation :: Conversation
initialConversation =
  Conversation
    { cRuns = [],
      cDraft = "",
      cReplacement = Nothing,
      cWaiting = False,
      cFailure = Nothing,
      cRunError = Nothing,
      cInputHistory = [],
      cHistoryIndex = Nothing,
      cSavedDraft = "",
      cServerModel = Nothing,
      cTokenStats = emptyTokenStats,
      cShowingTools = False,
      cScrollRequest = Nothing,
      cQuitting = False
    }

-- | Applies an action, with the run to start in the given project, if it
-- starts one.
updateConversation :: FilePath -> Action -> Conversation -> (Conversation, Maybe StartRun)
updateConversation dir action ui = case action of
  Network event -> (receive event ui, Nothing)
  -- Until the composer shows a replacement, its text is the one replaced.
  Edited value | isNothing (cReplacement ui) -> (ui {cDraft = value, cHistoryIndex = Nothing}, Nothing)
  Edited _ -> (ui, Nothing)
  Replaced -> (ui {cReplacement = Nothing}, Nothing)
  Send -> send dir ui
  Recall direction -> (recall direction ui, Nothing)
  CloseTools -> (ui {cShowingTools = False}, Nothing)
  ScrollTo scroll -> (ui {cScrollRequest = Just scroll}, Nothing)
  Scrolled -> (ui {cScrollRequest = Nothing}, Nothing)
  Exit -> (ui {cQuitting = True}, Nothing)
  _ -> (ui, Nothing)

receive :: NetworkEvent -> Conversation -> Conversation
receive event ui = case event of
  RunChanged run -> ui {cRuns = run : drop 1 (cRuns ui)}
  TurnDone -> ui {cWaiting = False}
  RunFailed err -> ui {cWaiting = False, cRunError = Just err}
  Disconnected err -> ui {cFailure = Just err, cWaiting = False}
  ServerModel m -> ui {cServerModel = Just m}
  ReplyRate r -> ui {cTokenStats = addRate (cTokenStats ui) r}
  _ -> ui

-- | Whether sending the draft would start a run: the server is connected
-- and idle, and the draft is not blank.
conversationCanSend :: Conversation -> Bool
conversationCanSend ui = not (cWaiting ui) && isNothing (cFailure ui) && not (T.null (T.strip (cDraft ui)))

send :: FilePath -> Conversation -> (Conversation, Maybe StartRun)
send dir ui = case parseCommand (cDraft ui) of
  Quit -> (ui {cQuitting = True}, Nothing)
  ListTools -> (sent {cShowingTools = True}, Nothing)
  RunRequirement req
    | conversationCanSend ui ->
        ( sent
            { cRuns = newRun req : cRuns ui,
              cWaiting = True,
              cRunError = Nothing,
              cShowingTools = False,
              cScrollRequest = Just ToLatest
            },
          Just (StartRun dir req)
        )
    | otherwise -> (ui, Nothing)
  where
    sent = (setDraft "" ui) {cInputHistory = cDraft ui : cInputHistory ui, cHistoryIndex = Nothing, cSavedDraft = ""}

recall :: Direction -> Conversation -> Conversation
recall direction ui
  | null sent = ui
  | otherwise = (setDraft value ui) {cHistoryIndex = if index < 0 then Nothing else Just index, cSavedDraft = saved}
  where
    sent = cInputHistory ui
    index = case direction of
      Older -> min (length sent - 1) (maybe 0 (+ 1) (cHistoryIndex ui))
      Newer -> maybe (-1) (subtract 1) (cHistoryIndex ui)
    saved = if isNothing (cHistoryIndex ui) then cDraft ui else cSavedDraft ui
    value = if index < 0 then saved else sent !! index

setDraft :: Text -> Conversation -> Conversation
setDraft value ui = ui {cDraft = value, cReplacement = Just value}

-- | What the conversationStatus line shows.
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

conversationStatus :: Conversation -> Status
conversationStatus ui = case cFailure ui of
  Just err -> Offline (safeText err)
  Nothing -> case (cWaiting ui, latest) of
    (True, Just (step, _)) -> Busy (stepLabel step <> progress step) (toolCallLabel <$> (run >>= latestToolCall))
    (True, Nothing) -> Busy "Starting" Nothing
    (False, Just (step, Failed _)) -> Stopped step
    _ -> Ready
  where
    run = listToMaybe (cRuns ui)
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

-- | A tool call as the conversationStatus line shows it: the tool's name and what it was
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

-- | Chat-local transcripts and drafts are kept apart from account settings.
data UI = UI
  { currentConversation :: Conversation,
    otherConversations :: Map.Map Int Conversation,
    currentChatId :: Int,
    nextChatId :: Int,
    chatModels :: Map.Map Int ModelChoice,
    modelChoices :: [ModelChoice],
    openAISettings :: SettingsSnapshot,
    settingsOpen :: Bool,
    settingsBusy :: Bool,
    settingsError :: Maybe Text
  } deriving (Eq, Show)

initialUI :: UI
initialUI = UI initialConversation Map.empty 0 1 Map.empty [] (SettingsSnapshot [] [] Nothing) False False Nothing

selectedChoice :: UI -> Maybe ModelChoice
selectedChoice ui = Map.lookup (currentChatId ui) (chatModels ui)

canSend :: UI -> Bool
canSend ui = conversationCanSend (currentConversation ui) && maybe False available (selectedChoice ui)
  where
    available choice = any (\c -> choiceModel c == choiceModel choice && choiceAccount c == choiceAccount choice) (modelChoices ui)

status :: UI -> Status
status = conversationStatus . currentConversation

update :: FilePath -> Action -> UI -> (UI, Maybe ClientTask)
update dir action ui = case action of
  OpenSettings -> (ui {settingsOpen = True}, Just RefreshSettings)
  CloseSettings -> (ui {settingsOpen = False}, Nothing)
  ConnectOpenAI fresh -> (ui {settingsError = Nothing, settingsBusy = True}, Just (SignIn fresh))
  DisconnectOpenAI -> (ui {settingsError = Nothing, settingsBusy = True}, Just SignOut)
  ChangeAccount account -> (ui {settingsBusy = True, settingsError = Nothing}, Just (SelectAccount account))
  RefreshModels -> (ui {settingsBusy = True, settingsError = Nothing}, Just RefreshSettings)
  CancelLogin -> (ui, Just CancelSignIn)
  NewChat ->
    let chat = nextChatId ui
        saved = Map.insert (currentChatId ui) (currentConversation ui) (otherConversations ui)
        choice = case selectedChoice ui of
          Just c | c `elem` modelChoices ui -> Just c
          _ -> listToMaybe (modelChoices ui)
        chosen = maybe (chatModels ui) (\c -> Map.insert chat c (chatModels ui)) choice
    in (ui {currentConversation = initialConversation {cReplacement = Just ""}, otherConversations = saved, currentChatId = chat, nextChatId = chat + 1, chatModels = chosen}, Nothing)
  SelectChat chat | chat /= currentChatId ui, Just conversation <- Map.lookup chat (otherConversations ui) ->
    (ui {currentConversation = conversation {cReplacement = Just (cDraft conversation), cScrollRequest = Just ToLatest},
         otherConversations = Map.insert (currentChatId ui) (currentConversation ui) (Map.delete chat (otherConversations ui)), currentChatId = chat}, Nothing)
  ChooseModel choice | not (waiting ui), choice `elem` modelChoices ui ->
    (ui {chatModels = Map.insert (currentChatId ui) choice (chatModels ui)}, Nothing)
  Network (ForChat chat event) ->
    if chat == currentChatId ui
      then (ui {currentConversation = receive event (currentConversation ui)}, Nothing)
      else (ui {otherConversations = Map.adjust (receive event) chat (otherConversations ui)}, Nothing)
  Network (ServerModels models) ->
    (withChoices ([ModelChoice m (modelId m) Nothing | m <- models] ++ filter (isJustAccount . choiceAccount) (modelChoices ui)) ui, Nothing)
  Network (SettingsLoaded snapshot) ->
    (withChoices (filter (isNothing . choiceAccount) (modelChoices ui) ++ openaiChoices snapshot) ui {openAISettings = snapshot, settingsError = catalogError snapshot}, Nothing)
  Network (SettingsBusy busy) -> (ui {settingsBusy = busy}, Nothing)
  Network (SettingsFailed err) -> (ui {settingsError = Just err}, Nothing)
  Network (Disconnected err) ->
    (ui {currentConversation = receive (Disconnected err) (currentConversation ui), otherConversations = Map.map (receive (Disconnected err)) (otherConversations ui)}, Nothing)
  Send | parseCommand (draft ui) /= Quit && parseCommand (draft ui) /= ListTools && not (canSend ui) -> (ui, Nothing)
  _ -> let (next, request) = updateConversation dir action (currentConversation ui)
           outgoing = do
             run <- request
             choice <- selectedChoice ui
             pure (SendToServer (StartChat (currentChatId ui) (choiceModel choice) (choiceAccount choice) run))
       in (ui {currentConversation = next}, outgoing)
  where
    isJustAccount (Just _) = True
    isJustAccount _ = False
    withChoices choices current =
      let chosen = if Map.member (currentChatId current) (chatModels current) then chatModels current
                   else maybe (chatModels current) (\c -> Map.insert (currentChatId current) c (chatModels current)) (listToMaybe (choices))
      in current {modelChoices = choices, chatModels = chosen}

updateAll :: FilePath -> [Action] -> UI -> (UI, [ClientTask])
updateAll dir actions ui = foldl apply (ui, []) actions
  where
    apply (current, started) action = let (next, task) = update dir action current in (next, started ++ maybeToList task)

chatSummaries :: UI -> [(Int, Text, Maybe ModelChoice, Bool)]
chatSummaries ui =
  [(key, maybe "New chat" (requirementText . requirement) (listToMaybe (reverse (cRuns conversation))), Map.lookup key (chatModels ui), cWaiting conversation)
   | (key, conversation) <- reverse (Map.toAscList (Map.insert (currentChatId ui) (currentConversation ui) (otherConversations ui)))]

runs :: UI -> [Run]
runs = cRuns . currentConversation

draft :: UI -> Text
draft = cDraft . currentConversation

replacement :: UI -> Maybe Text
replacement = cReplacement . currentConversation

waiting :: UI -> Bool
waiting = cWaiting . currentConversation

failure :: UI -> Maybe Text
failure = cFailure . currentConversation

inputHistory :: UI -> [Text]
inputHistory = cInputHistory . currentConversation

historyIndex :: UI -> Maybe Int
historyIndex = cHistoryIndex . currentConversation

savedDraft :: UI -> Text
savedDraft = cSavedDraft . currentConversation

serverModel :: UI -> Maybe Model
serverModel = fmap choiceModel . selectedChoice

tokenStats :: UI -> TokenStats
tokenStats = cTokenStats . currentConversation

showingTools :: UI -> Bool
showingTools = cShowingTools . currentConversation

scrollRequest :: UI -> Maybe Scroll
scrollRequest = cScrollRequest . currentConversation

quitting :: UI -> Bool
quitting = cQuitting . currentConversation

runError :: UI -> Maybe Text
runError = cRunError . currentConversation
