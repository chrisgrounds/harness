{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Core
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.Text as T
import System.Exit (exitFailure)
import Test.QuickCheck

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink = map T.pack . shrink . T.unpack

instance Arbitrary Requirement where
  arbitrary = Requirement <$> arbitrary

instance Arbitrary Step where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary StepStatus where
  arbitrary = oneof [pure Pending, pure Running, pure Passed, Failed <$> arbitrary]

instance Arbitrary TaskStatus where
  arbitrary = oneof [pure Queued, pure Working, pure Done, TaskFailed <$> arbitrary]

instance Arbitrary FunctionName where
  arbitrary = FunctionName <$> arbitrary

instance Arbitrary ModuleName where
  arbitrary = ModuleName <$> arbitrary

instance Arbitrary ComponentName where
  arbitrary = ComponentName <$> arbitrary

instance Arbitrary FunctionRef where
  arbitrary = FunctionRef <$> arbitrary <*> arbitrary

instance Arbitrary FunctionKind where
  arbitrary = elements [Designed, Repair]

instance Arbitrary Placeholder where
  arbitrary = elements [PreviousBody, Undefined]

instance Arbitrary FunctionSpec where
  arbitrary = FunctionSpec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Model where
  arbitrary = elements (allModels ++ map (OpenAIModel . T.pack) ["gpt-example", "future-model"])

instance Arbitrary ModulePlan where
  arbitrary = ModulePlan <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PropertySpec where
  arbitrary = PropertySpec <$> arbitrary <*> arbitrary

instance Arbitrary Artefact where
  arbitrary =
    Artefact <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ChangeKind where
  arbitrary = elements [NewFile, ExistingFile]

instance Arbitrary ChangedFile where
  arbitrary = ChangedFile <$> arbitrary <*> arbitrary

-- | Runs reachable from 'newRun', plus arbitrary contents in the other fields.
instance Arbitrary Run where
  arbitrary = do
    base <- newRun <$> arbitrary
    statuses <- vectorOf (length allSteps) arbitrary
    run <-
      (\i ts art files ws h -> base {iteration = i, tasks = ts, artefact = art, touchedFiles = files, warnings = ws, history = h})
        <$> chooseInt (1, 5)
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    pure run {steps = zip allSteps statuses}

instance Arbitrary ServerEvent where
  arbitrary = oneof [RunUpdated <$> arbitrary, pure TurnFinished]

roundTrips :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
roundTrips value = decode (encode value) == Just value

prop_runRoundTrips :: Run -> Bool
prop_runRoundTrips = roundTrips

prop_eventRoundTrips :: ServerEvent -> Bool
prop_eventRoundTrips = roundTrips

prop_requirementRoundTrips :: Requirement -> Bool
prop_requirementRoundTrips = roundTrips

prop_newRunIsPending :: Requirement -> Bool
prop_newRunIsPending req =
  let run = newRun req
   in map fst (steps run) == allSteps && all ((== Pending) . snd) (steps run) && iteration run == 1

prop_setStepChangesOnlyThatStep :: Step -> StepStatus -> Run -> Bool
prop_setStepChangesOnlyThatStep step status run =
  and
    [ if s == step then new == status else new == old
    | ((s, old), (_, new)) <- zip (steps run) (steps (setStep step status run))
    ]

prop_setTaskSetsAndKeepsOrder :: FunctionRef -> TaskStatus -> Run -> Bool
prop_setTaskSetsAndKeepsOrder name status run =
  let after = setTask name status run
      known = any ((== name) . fst) (tasks run)
   in lookup name (tasks after) == Just status
        && map fst (tasks after) == (if known then map fst (tasks run) else map fst (tasks run) ++ [name])

prop_restartFromResetsLaterSteps :: Step -> Run -> Bool
prop_restartFromResetsLaterSteps step run =
  let after = restartFrom step run
   in iteration after == iteration run + 1
        && and [if s >= step then new == Pending else new == old | ((s, old), (_, new)) <- zip (steps run) (steps after)]
        && (step > Implement || null (tasks after))

prop_currentStepIsActive :: Run -> Bool
prop_currentStepIsActive run = case currentStep run of
  Just (step, status) -> lookup step (steps run) == Just status && status `notElem` [Pending, Passed]
  Nothing -> all ((`elem` [Pending, Passed]) . snd) (steps run)

main :: IO ()
main = do
  results <-
    sequence
      [ check "Run JSON round-trips" prop_runRoundTrips,
        check "ServerEvent JSON round-trips" prop_eventRoundTrips,
        check "Requirement JSON round-trips" prop_requirementRoundTrips,
        check "newRun starts every step pending" prop_newRunIsPending,
        check "setStep changes only that step" prop_setStepChangesOnlyThatStep,
        check "setTask sets the task and keeps order" prop_setTaskSetsAndKeepsOrder,
        check "restartFrom resets that step and later ones" prop_restartFromResetsLaterSteps,
        check "currentStep is running or failed" prop_currentStepIsActive
      ]
  if and results then pure () else exitFailure
  where
    check :: (Testable p) => String -> p -> IO Bool
    check name p = do
      putStr (name <> ": ")
      isSuccess <$> quickCheckResult p
