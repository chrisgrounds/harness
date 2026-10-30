{-# LANGUAGE OverloadedStrings #-}

module ChatsTests (chatsTests) where

import Client.Chats
import Client.Settings (ModelChoice (..))
import Core hiding (Chat)
import Data.Aeson (decode, encode)
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.QuickCheck
import UI.State hiding (runs, inputHistory)
import qualified UI.State as State

chatsTests :: IO Bool
chatsTests =
  and
    <$> sequence
      [ check "saved chats survive JSON" prop_jsonRoundTrip,
        check "restoring saved chats gives back what was saved" prop_restoreRoundTrip,
        check "restoring selects the latest chat and skips other projects" prop_restoreSelectsLatest,
        check "a run in progress is stopped when restored" prop_restoreInterruptsRuns,
        check "unchanged chats keep their time" prop_restampKeepsUnchanged,
        check "draft edits and progress can wait to be saved" prop_urgentChange,
        check "saving replaces only the project's chats" prop_saveKeepsOtherProjects,
        check "the sidebar says briefly how long ago a chat changed" prop_ago
      ]
  where
    check :: (Testable p) => String -> p -> IO Bool
    check name p = do
      putStr (name <> ": ")
      isSuccess <$> quickCheckResult p

project :: FilePath
project = "/project"

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 10 30) 0

saved :: Int -> FilePath -> UTCTime -> SavedChat
saved chat dir time =
  SavedChat
    { chatId = chat,
      projectPath = dir,
      composerDraft = "half typed",
      modelChoice = Just (ModelChoice (OpenAIModel "example") "Example" (Just "account")),
      reasoningEffort = Just EffortHigh,
      runs = [setStep Discover Passed (newRun (Requirement "requirement"))],
      inputHistory = ["requirement"],
      lastUpdated = time
    }

prop_jsonRoundTrip :: Bool
prop_jsonRoundTrip = decode (encode chat) == Just chat
  where
    chat = saved 3 project epoch

prop_restoreRoundTrip :: Bool
prop_restoreRoundTrip =
  let chats = [saved 0 project epoch, saved 4 project (addUTCTime 1 epoch)]
   in chatsToSave project epoch (restoreChats project chats State.initialUI) == map (\c -> c {lastUpdated = epoch}) chats

prop_restoreSelectsLatest :: Bool
prop_restoreSelectsLatest =
  let chats = [saved 0 project epoch, saved 2 project (addUTCTime 1 epoch), saved 7 "/elsewhere" (addUTCTime 2 epoch)]
      ui = restoreChats project chats State.initialUI
      fresh = update project NewChat ui
   in currentChatId ui == 2 && draft ui == "half typed" && replacement ui == Just "half typed"
        && map summaryChat (chatSummaries ui) == [2, 0]
        && currentChatId (fst fresh) == 8
        && restoreChats project [] State.initialUI == State.initialUI

prop_restoreInterruptsRuns :: Bool
prop_restoreInterruptsRuns =
  let running = (saved 0 project epoch) {runs = [setStep DesignTypes Running (newRun (Requirement "requirement"))]}
      ui = restoreChats project [running] State.initialUI
   in not (waiting ui) && fmap fst (currentStep (head (State.runs ui))) == Just DesignTypes
        && status ui == Stopped DesignTypes

prop_restampKeepsUnchanged :: Bool
prop_restampKeepsUnchanged =
  let later = addUTCTime 60 epoch
      before = [saved 0 project epoch, saved 1 project epoch]
      after = [saved 0 project later, (saved 1 project later) {composerDraft = "edited"}]
   in map lastUpdated (restamp before after) == [epoch, later]

prop_urgentChange :: Bool
prop_urgentChange =
  let before = [saved 0 project epoch]
      chat = head before
      progressed = chat {runs = map (setTask (FunctionRef "src/M.hs" (FunctionName "f")) Working) (runs chat)}
      sent = chat {inputHistory = "another" : inputHistory chat}
      stepped = chat {runs = map (setStep DesignTypes Running) (runs chat)}
   in not (urgentChange before [chat {composerDraft = "edited"}])
        && not (urgentChange before [progressed])
        && all (urgentChange before) [[sent], [stepped], [], before ++ [saved 1 project epoch]]

prop_ago :: Bool
prop_ago =
  map (\seconds -> ago epoch (addUTCTime seconds epoch)) [0, 59, 60, 7 * 60, 3599, 3600, 2 * 3600, 86400, 3 * 86400, 7 * 86400, 40 * 86400]
    == ["now", "now", "1m", "7m", "59m", "1h", "2h", "1d", "3d", "1w", "5w"]

prop_saveKeepsOtherProjects :: Property
prop_saveKeepsOtherProjects = once $ ioProperty $ do
  temp <- getTemporaryDirectory
  (path, handle) <- openTempFile temp "chats-test"
  hClose handle
  removeFile path
  let file = path </> "harness" </> "chats.json"
      elsewhere = saved 1 "/elsewhere" epoch
  missing <- loadChats file
  saveChats file project [saved 0 project epoch]
  saveChats file "/elsewhere" [elsewhere]
  saveChats file project [saved 2 project epoch]
  loaded <- loadChats file
  removeDirectoryRecursive path
  pure (missing == Right [] && loaded == Right [elsewhere, saved 2 project epoch])
