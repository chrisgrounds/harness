{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Client (NetworkEvent (..), ClientTask (..))
import Client.Settings
import Client.Commands (Command (..), parseCommand)
import Core
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
typed value = fst . update project (Edited value)

act :: Action -> UI -> UI
act action = fst . update project action

-- | A draft that is neither blank nor a command.
newtype Requirement' = Requirement' T.Text
  deriving (Show)

instance Arbitrary Requirement' where
  arbitrary = Requirement' . T.pack <$> listOf1 (elements ['a' .. 'z'])

prop_sendStartsRunAndClears :: Requirement' -> Bool
prop_sendStartsRunAndClears (Requirement' value) =
  let (ui, started) = update project Send (typed value initialUI)
   in started == Just (SendToServer (StartChat 0 Mercury3 Nothing (StartRun project (Requirement value))))
        && waiting ui
        && draft ui == ""
        && replacement ui == Just ""
        && inputHistory ui == [value]
        && map requirement (runs ui) == [Requirement value]
        && scrollRequest ui == Just ToLatest

prop_sendWhileWaitingKeepsDraft :: Requirement' -> Requirement' -> Bool
prop_sendWhileWaitingKeepsDraft (Requirement' first) (Requirement' second) =
  let busy = typed second (act Replaced (act Send (typed first initialUI)))
      (ui, started) = update project Send busy
   in isNothing started && ui == busy

prop_blankDraftSendsNothing :: Property
prop_blankDraftSendsNothing =
  forAll (T.pack <$> listOf (elements " \n\t")) $ \blank ->
    let ui = typed blank initialUI in update project Send ui == (ui, Nothing)

prop_disconnectedSendsNothing :: Requirement' -> Bool
prop_disconnectedSendsNothing (Requirement' value) =
  isNothing (snd (update project Send (typed value (act (Network (Disconnected "gone")) initialUI))))

prop_commands :: Bool
prop_commands =
  let listed = act Send (typed " /tools " initialUI)
   in showingTools listed
        && null (runs listed)
        && draft listed == ""
        && not (showingTools (act CloseTools listed))
        && quitting (act Send (typed "/quit" initialUI))
        && parseCommand "/Tools" == RunRequirement (Requirement "/Tools")

-- | Recalling every sent message and coming back restores the draft.
prop_recallRestoresDraft :: [Requirement'] -> T.Text -> Bool
prop_recallRestoresDraft sent unsent =
  let afterSending = foldl (\ui (Requirement' value) -> act (Network TurnDone) (act Send (typed value (act Replaced ui)))) initialUI sent
      drafting = act Replaced (typed unsent (act Replaced afterSending))
      olders = iterate (act Replaced . act (Recall Older)) drafting
      oldest = olders !! length sent
      back = iterate (act Replaced . act (Recall Newer)) oldest !! length sent
   in draft back == unsent
        && case sent of
          [] -> draft oldest == unsent
          Requirement' first : _ -> draft oldest == first

prop_editsIgnoredUntilReplaced :: T.Text -> Bool
prop_editsIgnoredUntilReplaced stale =
  let cleared = act Send (typed "x" initialUI)
   in draft (typed stale cleared) == "" && draft (typed stale (act Replaced cleared)) == stale

prop_runChangedReplacesLatest :: Requirement' -> Bool
prop_runChangedReplacesLatest (Requirement' value) =
  let sent = act Send (typed value initialUI)
      progressed = setStep DesignTypes Running (newRun (Requirement value))
      ui = act (Network (RunChanged progressed)) sent
   in runs ui == [progressed]
        && status ui == Busy "Creating types" Nothing
        && runSummary progressed == "Creating types"
        && not (waiting (act (Network TurnDone) ui))

prop_scrollRequestsClear :: Bool
prop_scrollRequestsClear =
  let ui = act (ScrollTo PageUp) initialUI
   in scrollRequest ui == Just PageUp && isNothing (scrollRequest (act Scrolled ui))

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
      [ check "chats retain independent models, drafts and background updates" prop_chatIsolation,
        check "a model must be available before sending" prop_modelRequired,
        check "run errors allow retry without losing the connection" prop_runFailureRecoverable,
        check "sending starts a run and clears the draft" prop_sendStartsRunAndClears,
        check "sending while a run is in progress keeps the draft" prop_sendWhileWaitingKeepsDraft,
        check "a blank draft sends nothing" prop_blankDraftSendsNothing,
        check "nothing is sent once disconnected" prop_disconnectedSendsNothing,
        check "/tools and /quit" prop_commands,
        check "recall goes back through sent messages and restores the draft" prop_recallRestoresDraft,
        check "edits are ignored until a replacement is shown" prop_editsIgnoredUntilReplaced,
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
      first = act (ChooseModel (head choices)) base
      sent = act Send (typed "first requirement" first)
      second = act Replaced (act NewChat sent)
      picked = typed "unsent in second" (act (ChooseModel (choices !! 1)) second)
      progressed = setStep DesignTypes Running (newRun (Requirement "first requirement"))
      updated = act (Network (ForChat 0 (RunChanged progressed))) picked
      finished = act (Network (ForChat 0 TurnDone)) updated
      restored = act Replaced (act (SelectChat 0) finished)
      returned = act Replaced (act (SelectChat 1) restored)
   in null (runs finished) && draft finished == "unsent in second" && not (waiting finished)
        && selectedChoice restored == Just (head choices) && runs restored == [progressed] && not (waiting restored)
        && selectedChoice returned == Just (choices !! 1) && draft returned == "unsent in second"

prop_modelRequired :: Bool
prop_modelRequired =
  let ui = typed "requirement" State.initialUI
   in not (canSend ui) && isNothing (snd (update project Send ui))

prop_runFailureRecoverable :: Bool
prop_runFailureRecoverable =
  let failed = act Replaced (act (Network (ForChat 0 (RunFailed "limit reached"))) (act Send (typed "first" initialUI)))
   in isNothing (failure failed) && runError failed == Just "limit reached" && canSend (typed "retry" failed)
