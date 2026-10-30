{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Client (NetworkEvent (..), ClientTask (..))
import Client.Settings
import Client.Commands (Command (..), parseCommand)
import Core hiding (Chat)
import ChatsTests (chatsTests)
import MotionTests (motionTests)
import Data.Either (isLeft)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import Test.QuickCheck
import Theme.Palette (parsePalette)
import UI.State hiding (initialUI)
import qualified UI.State as State

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink = map T.pack . shrink . T.unpack

initialUI :: UI
initialUI = fst (update project (Network (ServerModels [Mercury3])) State.initialUI)

project :: FilePath
project = "/project"

-- | The state after typing a draft.
typed :: T.Text -> UI -> UI
typed value = chat (Edited value)

act :: Action -> UI -> UI
act action = fst . update project action

chat :: ChatAction -> UI -> UI
chat action = fst . applyChat action

applyChat :: ChatAction -> UI -> (UI, Maybe ClientTask)
applyChat action ui = update project (Chat (currentChatId ui) action) ui

ack :: UI -> UI
ack ui = maybe ui (\expected -> chat (Replaced expected) ui) (replacement ui)

-- | A draft that is neither blank nor a command.
newtype Requirement' = Requirement' T.Text
  deriving (Show)

instance Arbitrary Requirement' where
  arbitrary = Requirement' . T.pack <$> listOf1 (elements ['a' .. 'z'])

prop_sendStartsRunAndClears :: Requirement' -> Bool
prop_sendStartsRunAndClears (Requirement' value) =
  let (ui, started) = applyChat Send (typed value initialUI)
   in started == Just (SendToServer (StartChat 0 Mercury3 Nothing (StartRun project (Requirement value) Nothing)))
        && waiting ui
        && draft ui == ""
        && replacement ui == Just ""
        && inputHistory ui == [value]
        && map requirement (runs ui) == [Requirement value]
        && scrollRequest ui == Just ToLatest

prop_sendWhileWaitingKeepsDraft :: Requirement' -> Requirement' -> Bool
prop_sendWhileWaitingKeepsDraft (Requirement' first) (Requirement' second) =
  let busy = typed second (ack (chat Send (typed first initialUI)))
      (ui, started) = applyChat Send busy
   in isNothing started && ui == busy

prop_blankDraftSendsNothing :: Property
prop_blankDraftSendsNothing =
  forAll (T.pack <$> listOf (elements " \n\t")) $ \blank ->
    let ui = typed blank initialUI in applyChat Send ui == (ui, Nothing)

prop_disconnectedSendsNothing :: Requirement' -> Bool
prop_disconnectedSendsNothing (Requirement' value) =
  isNothing (snd (applyChat Send (typed value (act (Network (Disconnected "gone")) initialUI))))

-- | The socket belongs to the window, so neither a fresh chat nor returning
-- to an existing chat can restore a lost connection.
prop_disconnectionSurvivesNavigation :: Requirement' -> [Bool] -> Bool
prop_disconnectionSurvivesNavigation (Requirement' value) navigation =
  let first = chat Send (typed "first" initialUI)
      second = chat Send (typed "second" (ack (act NewChat first)))
      disconnected = act (Network (Disconnected "gone")) second
      actions = [NewChat, SelectChat 0, SelectChat 1]
        ++ map (\new -> if new then NewChat else SelectChat 0) navigation
      visited = scanl (flip act) disconnected actions
      blocked state =
        let drafting = typed value (ack state)
        in failure drafting == Just "gone"
             && status drafting == Offline "gone"
             && not (waiting drafting)
             && not (canSend drafting)
             && applyChat Send drafting == (drafting, Nothing)
  in all blocked visited

-- | Quit is an application request even when another action in the same
-- frame changes the selected conversation.
prop_exitSurvivesNavigation :: [Bool] -> Bool
prop_exitSurvivesNavigation navigation =
  let base = ack (act NewChat initialUI)
      actions = [NewChat, SelectChat 0, SelectChat 1]
        ++ map (\new -> if new then NewChat else SelectChat 0) navigation
      exiting = [act Exit base, chat Send (typed "/quit" base)]
      stopped state =
        let drafting = typed "after quit" (ack state)
        in quitting drafting && not (canSend drafting)
             && applyChat Send drafting == (drafting, Nothing)
  in all (all stopped . (\state -> scanl (flip act) state actions)) exiting

-- | Navigation and the old composer's acknowledgement can arrive in the
-- same frame. Edits, scroll acknowledgements and submission retain their
-- originating chat rather than affecting the newly selected draft.
prop_chatActionsStayWithOrigin :: Requirement' -> Bool
prop_chatActionsStayWithOrigin (Requirement' value) =
  let firstChoice = ModelChoice (OpenAIModel "first") "First" (Just "first-account")
      secondChoice = ModelChoice (OpenAIModel "second") "Second" (Just "second-account")
      base = act (Network (SettingsLoaded (SettingsSnapshot [] [firstChoice, secondChoice] Nothing))) State.initialUI
      sent = chat Send (typed "previous message" base)
      recalled = chat (Recall Older) (act (Network (ForChat 0 TurnDone)) (ack sent))
      actions = [NewChat, Chat 1 (ChooseModel secondChoice), Chat 0 (Replaced "previous message"),
                 Chat 0 (Edited value), Chat 0 (ScrollTo PageUp), Chat 0 Scrolled]
      (edited, editingTasks) = updateAll project actions recalled
      restored = act (SelectChat 0) edited
      (submitted, submissionTasks) = updateAll project (actions ++ [Chat 0 Send]) recalled
      submittedOrigin = act (SelectChat 0) submitted
  in null editingTasks && currentChatId edited == 1
       && draft edited == "" && replacement edited == Just ""
       && selectedChoice edited == Just secondChoice && null (runs edited)
       && draft restored == value && selectedChoice restored == Just firstChoice
       && isNothing (scrollRequest edited)
       && submissionTasks == [SendToServer (StartChat 0 (choiceModel firstChoice) (choiceAccount firstChoice) (StartRun project (Requirement value) Nothing))]
       && currentChatId submitted == 1 && replacement submitted == Just ""
       && not (waiting submitted) && null (runs submitted)
       && waiting submittedOrigin && draft submittedOrigin == ""
       && map requirement (runs submittedOrigin) == [Requirement value, Requirement "previous message"]

prop_backgroundSendChecksApplicationState :: Requirement' -> Bool
prop_backgroundSendChecksApplicationState (Requirement' value) =
  let ready = typed value initialUI
      unavailable = act (Network (ServerModels [])) ready
      cases = [act (Network (Disconnected "gone")) ready, act Exit ready,
               chat Send ready, unavailable]
      submit state = snd (updateAll project
        [NewChat, Chat 0 (Replaced ""), Chat 0 (Edited value), Chat 0 Send] state)
  in all (null . submit) cases

prop_reopeningSettingsPreservesLogin :: Bool
prop_reopeningSettingsPreservesLogin =
  let (reopened, started) = updateAll project [ConnectOpenAI False, CloseSettings, OpenSettings] initialUI
  in settingsOpen reopened && settingsBusy reopened && started == [SignIn False]
       && snd (update project OpenSettings initialUI) == Just RefreshSettings

prop_commands :: Bool
prop_commands =
  let listed = chat Send (typed " /tools " initialUI)
   in showingTools listed
        && null (runs listed)
        && draft listed == ""
        && not (showingTools (chat CloseTools listed))
        && quitting (chat Send (typed "/quit" initialUI))
        && parseCommand "/Tools" == RunRequirement (Requirement "/Tools")

-- | Recalling every sent message and coming back restores the draft.
prop_recallRestoresDraft :: [Requirement'] -> T.Text -> Bool
prop_recallRestoresDraft sent unsent =
  let afterSending = foldl (\ui (Requirement' value) -> act (Network TurnDone) (chat Send (typed value (ack ui)))) initialUI sent
      drafting = ack (typed unsent (ack afterSending))
      olders = iterate (ack . chat (Recall Older)) drafting
      oldest = olders !! length sent
      back = iterate (ack . chat (Recall Newer)) oldest !! length sent
   in draft back == unsent
        && case sent of
          [] -> draft oldest == unsent
          Requirement' first : _ -> draft oldest == first

prop_editsIgnoredUntilReplaced :: T.Text -> Bool
prop_editsIgnoredUntilReplaced stale =
  let cleared = chat Send (typed "x" initialUI)
   in draft (typed stale cleared) == "" && draft (typed stale (ack cleared)) == stale

-- | The composer can acknowledge last frame's replacement after a shortcut
-- requests an older message. That acknowledgement must not clear the new
-- replacement or let the composer's stale text overwrite it.
prop_staleReplacementAcknowledgement :: Bool
prop_staleReplacementAcknowledgement =
  let older = act (Network (ForChat 0 TurnDone)) (chat Send (typed "older" initialUI))
      newer = act (Network (ForChat 0 TurnDone)) (chat Send (typed "newer" (ack older)))
      recalled = chat (Recall Older) (ack newer)
      (next, started) = updateAll project
        [Chat 0 (Recall Older), Chat 0 (Replaced "newer"), Chat 0 (Edited "newer")] recalled
      editable = typed "edited older" (ack next)
  in null started && draft next == "older" && replacement next == Just "older"
       && historyIndex next == Just 1
       && draft editable == "edited older" && isNothing (replacement editable)

prop_runChangedReplacesLatest :: Requirement' -> Bool
prop_runChangedReplacesLatest (Requirement' value) =
  let sent = chat Send (typed value initialUI)
      progressed = setStep DesignTypes Running (newRun (Requirement value))
      ui = act (Network (RunChanged progressed)) sent
   in runs ui == [progressed]
        && status ui == Busy "Creating types" Nothing
        && runSummary progressed == "Creating types"
        && not (waiting (act (Network TurnDone) ui))

prop_scrollRequestsClear :: Bool
prop_scrollRequestsClear =
  let ui = chat (ScrollTo PageUp) initialUI
   in scrollRequest ui == Just PageUp && isNothing (scrollRequest (chat Scrolled ui))

-- | A run that ends says so, and one that fails says where.
prop_runSummary :: Bool
prop_runSummary =
  let run = newRun (Requirement "x")
      passed = foldr (`setStep` Passed) run allSteps
   in runSummary passed == "Done"
        && runSummary (setStep TypeCheck (Failed "no") passed) == "Stopped at type checking"
        && runSummary run == "Waiting"
        && rateText 24.6 == "25 tokens a second"

prop_packagedPaletteParses :: Property
prop_packagedPaletteParses = ioProperty $ do
  contents <- TIO.readFile "themes/orng.toml"
  pure (either (const False) (const True) (parsePalette contents))

prop_paletteNamesMissingColour :: Bool
prop_paletteNamesMissingColour =
  parsePalette "background = [1, 2, 3]" == Left "missing colour text"
    && isLeft (parsePalette "background = [1, 2, 300]")
    && isJust (either (const Nothing) Just (parsePalette (T.unlines [k <> " = [0, 0, 0]" | k <- keys])))
  where
    keys = ["background", "text", "orange", "secondaryOrange", "cyan", "muted", "border", "error", "success"]

main :: IO ()
main = do
  results <-
    sequence
      [ motionTests,
        chatsTests,
        check "chats retain independent models, drafts and background updates" prop_chatIsolation,
        check "OpenAI effort stays with its chat and travels with requests" prop_chatEffort,
        check "sidebar history contains only chats that have started" prop_historyContainsOnlyStartedChats,
        check "a model must be available before sending" prop_modelRequired,
        check "run errors allow retry without losing the connection" prop_runFailureRecoverable,
        check "sending starts a run and clears the draft" prop_sendStartsRunAndClears,
        check "sending while a run is in progress keeps the draft" prop_sendWhileWaitingKeepsDraft,
        check "a blank draft sends nothing" prop_blankDraftSendsNothing,
        check "nothing is sent once disconnected" prop_disconnectedSendsNothing,
        check "disconnection survives new chats and chat switching" prop_disconnectionSurvivesNavigation,
        check "quit survives new chats and chat switching" prop_exitSurvivesNavigation,
        check "batched component actions retain their originating chat" prop_chatActionsStayWithOrigin,
        check "background sends respect application and chat availability" prop_backgroundSendChecksApplicationState,
        check "reopening settings preserves an in-flight login" prop_reopeningSettingsPreservesLogin,
        check "/tools and /quit" prop_commands,
        check "recall goes back through sent messages and restores the draft" prop_recallRestoresDraft,
        check "edits are ignored until a replacement is shown" prop_editsIgnoredUntilReplaced,
        check "stale composer acknowledgements preserve newly recalled text" prop_staleReplacementAcknowledgement,
        check "a run update replaces the latest run" prop_runChangedReplacesLatest,
        check "scroll requests clear once made" prop_scrollRequestsClear,
        check "run summaries" prop_runSummary,
        check "the packaged palette parses" prop_packagedPaletteParses,
        check "a palette names its missing colour" prop_paletteNamesMissingColour
      ]
  if and results then pure () else exitFailure
  where
    check :: (Testable p) => String -> p -> IO Bool
    check name p = do
      putStr (name <> ": ")
      isSuccess <$> quickCheckResult p

prop_chatIsolation :: Bool
prop_chatIsolation =
  let choices = [ModelChoice (OpenAIModel "first") "First" (Just "account"), ModelChoice (OpenAIModel "second") "Second" (Just "account")]
      base = act (Network (SettingsLoaded (SettingsSnapshot [] choices Nothing))) initialUI
      first = chat (ChooseModel (head choices)) base
      sent = chat Send (typed "first requirement" first)
      second = ack (act NewChat sent)
      picked = typed "unsent in second" (chat (ChooseModel (choices !! 1)) second)
      progressed = setStep DesignTypes Running (newRun (Requirement "first requirement"))
      updated = act (Network (ForChat 0 (RunChanged progressed))) picked
      finished = act (Network (ForChat 0 TurnDone)) updated
      restored = ack (act (SelectChat 0) finished)
      returned = ack (act (SelectChat 1) restored)
   in null (runs finished) && draft finished == "unsent in second" && not (waiting finished)
        && selectedChoice restored == Just (head choices) && runs restored == [progressed] && not (waiting restored)
        && selectedChoice returned == Just (choices !! 1) && draft returned == "unsent in second"

-- | Drafts are retained without becoming sidebar history. Starting a run
-- makes the originating chat visible even while a different draft is open.
prop_historyContainsOnlyStartedChats :: Requirement' -> Requirement' -> Bool
prop_historyContainsOnlyStartedChats (Requirement' first) (Requirement' second) =
  let drafted = typed first initialUI
      fresh = typed second (ack (act NewChat drafted))
      started = chat Send fresh
      background = typed "another draft" (ack (act NewChat started))
      (bothStarted, outgoing) = update project (Chat 0 Send) background
      finished = act (Network (ForChat 0 TurnDone)) bothStarted
      expectedChoice = selectedChoice initialUI
  in null (chatSummaries State.initialUI) && null (chatSummaries initialUI)
       && null (chatSummaries drafted) && null (chatSummaries fresh)
       && draft (act (SelectChat 0) fresh) == first
       && listed started == [(1, second, expectedChoice, True)]
       && chatSummaries background == chatSummaries started
       && outgoing == Just (SendToServer (StartChat 0 Mercury3 Nothing (StartRun project (Requirement first) Nothing)))
       && currentChatId bothStarted == 2 && draft bothStarted == "another draft"
       && listed bothStarted == [(1, second, expectedChoice, True), (0, first, expectedChoice, True)]
       && listed finished == [(1, second, expectedChoice, True), (0, first, expectedChoice, False)]
  where
    listed = map (\s -> (summaryChat s, summaryTitle s, summaryChoice s, summaryBusy s)) . chatSummaries

prop_chatEffort :: Property
prop_chatEffort = forAll (elements (Nothing : map Just [minBound .. maxBound])) $ \effort ->
  let choice = ModelChoice (OpenAIModel "example") "Example" (Just "account")
      base = act (Network (SettingsLoaded (SettingsSnapshot [] [choice] Nothing))) initialUI
      picked = chat (ChooseEffort effort) (chat (ChooseModel choice) base)
      fresh = ack (act NewChat picked)
      changed = chat (ChooseEffort (Just EffortLow)) fresh
      restored = act (SelectChat 0) changed
      (busy, outgoing) = applyChat Send (typed "requirement" (ack restored))
      busyUnchanged = chat (ChooseEffort (Just EffortHigh)) busy == busy
      defaulted = chat (ChooseEffort Nothing) (ack restored)
      switched = chat (ChooseModel (ModelChoice Mercury3 "mercury-3" Nothing)) (ack restored)
      otherRequest = snd (applyChat Send (typed "other" switched))
      -- Same-frame navigation still sends the originating chat's effort.
      (_, backgroundTasks) = updateAll project
        [NewChat, Chat 0 (Replaced ""), Chat 0 (Edited "background"), Chat 0 Send] restored
  in selectedEffort picked == effort && selectedEffort restored == effort
       && selectedEffort fresh == Nothing && selectedEffort changed == Just EffortLow
       && outgoing == Just (SendToServer (StartChat 0 (choiceModel choice) (choiceAccount choice)
            (StartRun project (Requirement "requirement") effort)))
       && busyUnchanged && selectedEffort defaulted == Nothing
       && chat (ChooseEffort (Just EffortHigh)) switched == switched
       && otherRequest == Just (SendToServer (StartChat 0 Mercury3 Nothing (StartRun project (Requirement "other") Nothing)))
       && backgroundTasks == [SendToServer (StartChat 0 (choiceModel choice) (choiceAccount choice)
            (StartRun project (Requirement "background") effort))]

prop_modelRequired :: Bool
prop_modelRequired =
  let ui = typed "requirement" State.initialUI
   in not (canSend ui) && isNothing (snd (applyChat Send ui))

prop_runFailureRecoverable :: Bool
prop_runFailureRecoverable =
  let failed = ack (act (Network (ForChat 0 (RunFailed "limit reached"))) (chat Send (typed "first" initialUI)))
   in isNothing (failure failed) && runError failed == Just "limit reached" && canSend (typed "retry" failed)
