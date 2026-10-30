{-# LANGUAGE OverloadedStrings #-}

-- | The interface's state and how it changes, with no drawing: the window
-- turns what the user does into 'Action's and draws the state that results.
module UI.State
  ( UI (..), Conversation, Scroll (..), Direction (..), Action (..), ChatAction (..),
    initialUI, update, updateAll, canSend, Status (..), status,
    implementCounts, target, rateText, runSummary, toolCallLabel, toolCallsLabel, allToolNames,
    runs, draft, replacement, waiting, failure, runError, inputHistory, historyIndex,
    savedDraft, serverModel, tokenStats, showingTools, scrollRequest, quitting,
    ChatSummary (..), chatSummaries, chatTitle, ago, selectedChoice, selectedEffort, restoreChats, chatsToSave, RenameEditor (..)
  ) where

import Client (NetworkEvent (..), ClientTask (..), safeText)
import Client.Chats (SavedChat (SavedChat))
import qualified Client.Chats as Saved
import Client.Settings
import qualified Data.Map.Strict as Map
import Client.Commands (Command (..), parseCommand)
import Client.Tokens (TokenStats, addRate, emptyTokenStats)
import Core hiding (Chat)
import Data.List (sortOn)
import Data.Maybe (isNothing, listToMaybe, maybeToList)
import Data.Ord (Down (..))
import Data.Time (UTCTime, diffUTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Core.RunTime (freezeRunTiming)

data Conversation = Conversation
  { cRuns :: [Run], cDraft :: Text, cReplacement :: Maybe Text,
    cWaiting :: Bool, cRunError :: Maybe Text, cInputHistory :: [Text],
    cHistoryIndex :: Maybe Int, cSavedDraft :: Text,
    cModelChoice :: Maybe ModelChoice, cReasoningEffort :: Maybe ReasoningEffort,
    cTokenStats :: TokenStats, cShowingTools :: Bool,
    cScrollRequest :: Maybe Scroll,
    cCustomTitle :: Maybe Text
  } deriving (Eq, Show)

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

-- | An interaction with one conversation. The window attaches its snapshot's
-- chat ID so actions still reach their owner when navigation comes first in
-- the same frame.
data ChatAction
  = -- | The user changed the composer's text.
    Edited Text
  | -- | The composer shows this replacement from the frame's snapshot.
    Replaced Text
  | Send
  | -- | Show an older or newer sent message in the composer.
    Recall Direction
  | CloseTools
  | ScrollTo Scroll
  | -- | The transcript made the requested scroll.
    Scrolled
  | ChooseModel ModelChoice
  | ChooseEffort (Maybe ReasoningEffort)
  deriving (Eq, Show)

data Action
  = Network NetworkEvent | Chat Int ChatAction | Exit | NewChat | SelectChat Int
  | OpenSettings | CloseSettings | ConnectOpenAI Bool | DisconnectOpenAI
  | ChangeAccount Text | RefreshModels | CancelLogin
  | BeginRename Int | EditRename Text | SaveRename | CancelRename | DeleteChat Int
  deriving (Eq, Show)

initialConversation :: Conversation
initialConversation =
  Conversation
    { cRuns = [],
      cDraft = "",
      cReplacement = Nothing,
      cWaiting = False,
      cRunError = Nothing,
      cInputHistory = [],
      cHistoryIndex = Nothing,
      cSavedDraft = "",
      cModelChoice = Nothing,
      cReasoningEffort = Nothing,
      cTokenStats = emptyTokenStats,
      cShowingTools = False,
      cScrollRequest = Nothing,
      cCustomTitle = Nothing
    }

-- | Applies an action, with the run to start in the given project, if it
-- starts one.
updateConversation :: FilePath -> ChatAction -> Conversation -> (Conversation, Maybe StartRun)
updateConversation dir action ui = case action of
  -- Until the composer shows a replacement, its text is the one replaced.
  Edited value | isNothing (cReplacement ui) -> (ui {cDraft = value, cHistoryIndex = Nothing}, Nothing)
  Edited _ -> (ui, Nothing)
  Replaced expected | cReplacement ui == Just expected -> (ui {cReplacement = Nothing}, Nothing)
  Replaced _ -> (ui, Nothing)
  Send -> send dir ui
  Recall direction -> (recall direction ui, Nothing)
  CloseTools -> (ui {cShowingTools = False}, Nothing)
  ScrollTo scroll -> (ui {cScrollRequest = Just scroll}, Nothing)
  Scrolled -> (ui {cScrollRequest = Nothing}, Nothing)
  ChooseModel choice -> (ui {cModelChoice = Just choice}, Nothing)
  ChooseEffort effort -> (ui {cReasoningEffort = effort}, Nothing)

receive :: NetworkEvent -> Conversation -> Conversation
receive event ui = case event of
  RunChanged run -> ui {cRuns = run : drop 1 (cRuns ui)}
  TurnDone -> ui {cWaiting = False}
  RunFailed err -> ui {cWaiting = False, cRunError = Just err}
  Disconnected _ -> ui {cWaiting = False}
  ReplyRate r -> ui {cTokenStats = addRate (cTokenStats ui) r}
  _ -> ui

-- | The chat is idle and has a nonblank draft. Connection and model
-- availability are checked by 'canSend' at application level.
conversationCanSend :: Conversation -> Bool
conversationCanSend ui = not (cWaiting ui) && not (T.null (T.strip (cDraft ui)))

send :: FilePath -> Conversation -> (Conversation, Maybe StartRun)
send dir ui = case parseCommand (cDraft ui) of
  Quit -> (ui, Nothing) -- Handled by the application update.
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
          Just (StartRun dir req (case choiceModel <$> cModelChoice ui of
            Just (OpenAIModel _) -> cReasoningEffort ui
            _ -> Nothing))
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
conversationStatus ui = case (cWaiting ui, latest) of
  (True, Just (step, _)) -> Busy (stepLabel step <> progress step) (toolCallsLabel . activeToolCalls =<< run)
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

-- | The tool calls still running as the conversationStatus line shows them: the
-- newest, then how many others, e.g. @ReadFile a.hs  +2 more@.
toolCallsLabel :: [ToolCall] -> Maybe Text
toolCallsLabel [] = Nothing
toolCallsLabel calls = Just (toolCallLabel (last calls) <> (if others > 0 then "  +" <> T.pack (show others) <> " more" else ""))
  where
    others = length calls - 1

-- | The name of every tool the harness can call, in declaration order.
allToolNames :: [Text]
allToolNames = map toolNameText [minBound .. maxBound]

-- | Application lifetime, connection and account settings are shared by all
-- chats. Each conversation owns its transcript, draft and model choice.
data UI = UI
  { currentConversation :: Conversation, otherConversations :: Map.Map Int Conversation,
    currentChatId :: Int, nextChatId :: Int, connectionFailure :: Maybe Text,
    exitRequested :: Bool, modelChoices :: [ModelChoice],
    openAISettings :: SettingsSnapshot, settingsOpen :: Bool,
    settingsBusy :: Bool, settingsError :: Maybe Text,
    renameEditor :: Maybe RenameEditor
  } deriving (Eq, Show)

initialUI :: UI
initialUI = UI
  { currentConversation = initialConversation,
    otherConversations = Map.empty,
    currentChatId = 0,
    nextChatId = 1,
    connectionFailure = Nothing,
    exitRequested = False,
    modelChoices = [],
    openAISettings = SettingsSnapshot [] [] Nothing,
    settingsOpen = False,
    settingsBusy = False,
    settingsError = Nothing,
    renameEditor = Nothing
  }

selectedChoice :: UI -> Maybe ModelChoice
selectedChoice = cModelChoice . currentConversation

selectedEffort :: UI -> Maybe ReasoningEffort
selectedEffort = cReasoningEffort . currentConversation

canSend :: UI -> Bool
canSend ui = canSendConversation ui (currentConversation ui)

canSendConversation :: UI -> Conversation -> Bool
canSendConversation ui conversation = isNothing (failure ui) && not (quitting ui)
  && conversationCanSend conversation && maybe False available (cModelChoice conversation)
  where
    available choice = any (\c -> choiceModel c == choiceModel choice && choiceAccount c == choiceAccount choice) (modelChoices ui)

status :: UI -> Status
status ui = case failure ui of
  Just err -> Offline (safeText err)
  Nothing -> conversationStatus (currentConversation ui)

update :: FilePath -> Action -> UI -> (UI, Maybe ClientTask)
update dir action ui = case action of
  BeginRename chat -> case lookupChat chat of
    Nothing -> (ui, Nothing)
    Just conversation ->
      let title = case cCustomTitle conversation of
            Just t -> t
            Nothing -> maybe "" (requirementText . requirement) (listToMaybe (reverse (cRuns conversation)))
      in (ui {renameEditor = Just (RenameEditor chat title)}, Nothing)
  EditRename value -> (ui {renameEditor = fmap (\editor -> editor {renameDraft = value}) (renameEditor ui)}, Nothing)
  CancelRename -> (ui {renameEditor = Nothing}, Nothing)
  SaveRename -> case renameEditor ui of
    Nothing -> (ui, Nothing)
    Just editor -> case lookupChat (renameChatId editor) of
      Nothing -> (ui {renameEditor = Nothing}, Nothing)
      Just conversation
        | T.null (T.strip (renameDraft editor)) -> (ui, Nothing)
        | otherwise ->
            let renamed = conversation {cCustomTitle = Just (T.strip (renameDraft editor))}
                updated = if renameChatId editor == currentChatId ui
                  then ui {currentConversation = renamed}
                  else ui {otherConversations = Map.insert (renameChatId editor) renamed (otherConversations ui)}
            in (updated {renameEditor = Nothing}, Nothing)
  DeleteChat chat -> case lookupChat chat of
    Nothing -> (ui, Nothing)
    Just _ ->
      let cleared = ui {renameEditor = case renameEditor ui of
                         Just editor | renameChatId editor == chat -> Nothing
                         editor -> editor}
      in if chat /= currentChatId ui
         then (cleared {otherConversations = Map.delete chat (otherConversations ui)}, Nothing)
         else case Map.maxViewWithKey (Map.delete chat (otherConversations ui)) of
           Just ((key, conversation), remaining) ->
             (cleared {currentChatId = key, otherConversations = remaining,
                       currentConversation = conversation {cReplacement = Just (cDraft conversation), cScrollRequest = Just ToLatest}}, Nothing)
           Nothing ->
             (cleared {currentChatId = nextChatId ui, nextChatId = nextChatId ui + 1,
                       otherConversations = Map.empty,
                       currentConversation = initialConversation {cReplacement = Just "", cModelChoice = listToMaybe (modelChoices ui)}}, Nothing)
  Exit -> (ui {exitRequested = True}, Nothing)
  Chat chat chatAction -> updateChat dir chat chatAction ui
  OpenSettings -> (ui {settingsOpen = True}, if settingsBusy ui then Nothing else Just RefreshSettings)
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
    in (ui {currentConversation = initialConversation {cReplacement = Just "", cModelChoice = choice}, otherConversations = saved, currentChatId = chat, nextChatId = chat + 1}, Nothing)
  SelectChat chat | chat /= currentChatId ui, Just conversation <- Map.lookup chat (otherConversations ui) ->
    (ui {currentConversation = conversation {cReplacement = Just (cDraft conversation), cScrollRequest = Just ToLatest},
         otherConversations = Map.insert (currentChatId ui) (currentConversation ui) (Map.delete chat (otherConversations ui)), currentChatId = chat}, Nothing)
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
    (ui {connectionFailure = Just err, currentConversation = receive (Disconnected err) (currentConversation ui), otherConversations = Map.map (receive (Disconnected err)) (otherConversations ui)}, Nothing)
  Network event -> (ui {currentConversation = receive event (currentConversation ui)}, Nothing)
  SelectChat _ -> (ui, Nothing)
  where
    lookupChat chat
      | chat == currentChatId ui = Just (currentConversation ui)
      | otherwise = Map.lookup chat (otherConversations ui)
    isJustAccount (Just _) = True
    isJustAccount _ = False
    withChoices choices current =
      let conversation = currentConversation current
          chosen = case cModelChoice conversation of
            Just choice -> Just choice
            Nothing -> listToMaybe choices
      in current {modelChoices = choices, currentConversation = conversation {cModelChoice = chosen}}

-- | Apply a component's actions to the chat that produced them, even if a
-- preceding action selected another chat. Availability and exit checks stay
-- application-wide; requests retain the originating chat's model and account.
updateChat :: FilePath -> Int -> ChatAction -> UI -> (UI, Maybe ClientTask)
updateChat dir chat action ui = case conversation of
  Nothing -> (ui, Nothing)
  Just current -> case action of
    Send | parseCommand (cDraft current) == Quit -> (ui {exitRequested = True}, Nothing)
    Send | parseCommand (cDraft current) /= ListTools && not (canSendConversation ui current) -> (ui, Nothing)
    ChooseModel choice | cWaiting current || choice `notElem` modelChoices ui -> (ui, Nothing)
    ChooseEffort _ | cWaiting current || maybe True ((/= OpenAI) . provider . choiceModel) (cModelChoice current) -> (ui, Nothing)
    _ ->
      let (next, request) = updateConversation dir action current
          outgoing = do
            run <- request
            choice <- cModelChoice current
            pure (SendToServer (StartChat chat (choiceModel choice) (choiceAccount choice) run))
          updated = if chat == currentChatId ui
            then ui {currentConversation = next}
            else ui {otherConversations = Map.insert chat next (otherConversations ui)}
      in (updated, outgoing)
  where
    conversation = if chat == currentChatId ui
      then Just (currentConversation ui)
      else Map.lookup chat (otherConversations ui)

updateAll :: FilePath -> [Action] -> UI -> (UI, [ClientTask])
updateAll dir actions ui = foldl apply (ui, []) actions
  where
    apply (current, started) action = let (next, task) = update dir action current in (next, started ++ maybeToList task)

-- | A chat as the sidebar lists it.
data ChatSummary = ChatSummary
  { summaryChat :: Int,
    -- | The chat's first requirement.
    summaryTitle :: Text,
    summaryChoice :: Maybe ModelChoice,
    summaryBusy :: Bool,
    summaryLatest :: Run
  }
  deriving (Eq, Show)

-- | Session history contains chats that have started a run, newest first.
-- Unsent drafts retain their state but do not appear as history entries.
chatSummaries :: UI -> [ChatSummary]
chatSummaries ui =
  [ChatSummary key title (cModelChoice conversation) (cWaiting conversation) latest
   | (key, conversation) <- reverse (Map.toAscList (Map.insert (currentChatId ui) (currentConversation ui) (otherConversations ui))),
     latest : _ <- [cRuns conversation],
     Just title <- [conversationTitle conversation]]

-- | The current chat's name, once it has started a run.
chatTitle :: UI -> Maybe Text
chatTitle = conversationTitle . currentConversation

-- | A chat's custom title, or else its first requirement, once it has
-- started a run.
conversationTitle :: Conversation -> Maybe Text
conversationTitle conversation = case listToMaybe (reverse (cRuns conversation)) of
  Nothing -> Nothing
  Just oldest -> Just (maybe (requirementText (requirement oldest)) id (cCustomTitle conversation))

-- | How long ago the first time was before the second, briefly: @now@,
-- @7m@, @2h@, @3d@ or @5w@.
ago :: UTCTime -> UTCTime -> Text
ago before now
  | minutes < 1 = "now"
  | minutes < 60 = T.pack (show minutes) <> "m"
  | hours < 24 = T.pack (show hours) <> "h"
  | days < 7 = T.pack (show days) <> "d"
  | otherwise = T.pack (show (days `div` 7)) <> "w"
  where
    minutes = floor (realToFrac (diffUTCTime now before) / 60 :: Double) :: Int
    hours = minutes `div` 60
    days = hours `div` 24

-- | The project's saved chats in place of the initial one: the most
-- recently updated is selected, and the others kept if they started a run.
-- A run left in progress when the app closed is stopped where it was.
restoreChats :: FilePath -> [SavedChat] -> UI -> UI
restoreChats dir saved ui = case sortOn (Down . Saved.lastUpdated) [c | c <- saved, Saved.projectPath c == dir] of
  [] -> ui
  latest : others ->
    ui {currentConversation = (restore latest) {cReplacement = Just (Saved.composerDraft latest), cScrollRequest = Just ToLatest},
        otherConversations = Map.fromList [(Saved.chatId c, restore c) | c <- others, not (null (Saved.runs c))],
        currentChatId = Saved.chatId latest,
        nextChatId = maximum (nextChatId ui : map ((+ 1) . Saved.chatId) saved),
        renameEditor = Nothing}
  where
    restore c = initialConversation
      { cRuns = map interrupt (Saved.runs c),
        cDraft = Saved.composerDraft c,
        cInputHistory = Saved.inputHistory c,
        cSavedDraft = Saved.composerDraft c,
        cModelChoice = Saved.modelChoice c,
        cReasoningEffort = Saved.reasoningEffort c,
        cCustomTitle = Saved.customTitle c
      }
    interrupt run =
      (foldr (\(step, s) r -> if s == Running then setStep step (Failed "Interrupted when the app closed") r else r) run (steps run))
        {runTiming = freezeRunTiming (runTiming run)}

-- | The chats worth keeping, all stamped with the given time: those that
-- started a run or have a draft.
chatsToSave :: FilePath -> UTCTime -> UI -> [SavedChat]
chatsToSave dir now ui =
  [ SavedChat chat dir (cDraft c) (cModelChoice c) (cReasoningEffort c) (cRuns c) (cInputHistory c) now (cCustomTitle c)
  | (chat, c) <- Map.toAscList (Map.insert (currentChatId ui) (currentConversation ui) (otherConversations ui)),
    not (null (cRuns c)) || not (T.null (T.strip (cDraft c)))
  ]

runs :: UI -> [Run]
runs = cRuns . currentConversation

draft :: UI -> Text
draft = cDraft . currentConversation

replacement :: UI -> Maybe Text
replacement = cReplacement . currentConversation

waiting :: UI -> Bool
waiting = cWaiting . currentConversation

failure :: UI -> Maybe Text
failure = connectionFailure

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
quitting = exitRequested

runError :: UI -> Maybe Text
runError = cRunError . currentConversation

data RenameEditor = RenameEditor { renameChatId :: Int, renameDraft :: Text } deriving (Eq, Show)
