{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Core.Run
import Test.QuickCheck
import qualified Test.QuickCheck as HoleyQuickCheck
import qualified System.Exit as HoleyExit
import Core
import Data.Aeson (FromJSON, ToJSON, Value (..), decode, encode, toJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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

instance Arbitrary ToolName where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ToolCall where
  arbitrary = ToolCall <$> arbitrary <*> arbitrary

instance Arbitrary ToolRequest where
  arbitrary = ToolRequest <$> arbitrary <*> pure FunctionTool <*> (ToolRequestFunctionParams <$> arbitrary <*> arbitrary)

-- | Runs reachable from 'newRun', plus arbitrary contents in the other fields.
instance Arbitrary Run where
  arbitrary = do
    base <- newRun <$> arbitrary
    statuses <- vectorOf (length allSteps) arbitrary
    run <-
      (\i ts art files ws h tc -> base {iteration = i, tasks = ts, artefact = art, touchedFiles = files, warnings = ws, history = h, activeToolCalls = tc})
        <$> chooseInt (1, 5)
        <*> arbitrary
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

prop_newRunNoToolCall :: Requirement -> Bool
prop_newRunNoToolCall req = null (activeToolCalls (newRun req))

prop_startToolCallAppends :: ToolRequest -> Run -> Bool
prop_startToolCallAppends t run = activeToolCalls (startToolCall t run) == activeToolCalls run ++ [toolCallOf t]

prop_finishUndoesStart :: ToolRequest -> Run -> Bool
prop_finishUndoesStart t run = length (activeToolCalls (finishToolCall t (startToolCall t run))) == length (activeToolCalls run)

prop_runWithoutActiveCallsDecodes :: Requirement -> Bool
prop_runWithoutActiveCallsDecodes req = decode (encode (KeyMap.delete (Key.fromString "activeToolCalls") object)) == Just (newRun req)
  where
    object = case toJSON (newRun req) of
      Object o -> o
      _ -> KeyMap.empty

prop_toolCallHasPath :: T.Text -> Bool
prop_toolCallHasPath path =
  toolCallOf (ToolRequest (T.pack "1") FunctionTool (ToolRequestFunctionParams ReadFile (T.pack "{\"path\": " <> TL.toStrict (encodeToLazyText path) <> T.pack "}")))
    == ToolCall ReadFile (Just path)

main :: IO ()
main = do
  results <-
    sequence
      [ putStrLn "=== prop_runRoundTrips" >> HoleyQuickCheck.quickCheckResult prop_runRoundTrips
      , putStrLn "=== prop_eventRoundTrips" >> HoleyQuickCheck.quickCheckResult prop_eventRoundTrips
      , putStrLn "=== prop_requirementRoundTrips" >> HoleyQuickCheck.quickCheckResult prop_requirementRoundTrips
      , putStrLn "=== prop_newRunIsPending" >> HoleyQuickCheck.quickCheckResult prop_newRunIsPending
      , putStrLn "=== prop_setStepChangesOnlyThatStep" >> HoleyQuickCheck.quickCheckResult prop_setStepChangesOnlyThatStep
      , putStrLn "=== prop_setTaskSetsAndKeepsOrder" >> HoleyQuickCheck.quickCheckResult prop_setTaskSetsAndKeepsOrder
      , putStrLn "=== prop_restartFromResetsLaterSteps" >> HoleyQuickCheck.quickCheckResult prop_restartFromResetsLaterSteps
      , putStrLn "=== prop_currentStepIsActive" >> HoleyQuickCheck.quickCheckResult prop_currentStepIsActive
      , putStrLn "=== prop_newRunNoToolCall" >> HoleyQuickCheck.quickCheckResult prop_newRunNoToolCall
      , putStrLn "=== prop_startToolCallAppends" >> HoleyQuickCheck.quickCheckResult prop_startToolCallAppends
      , putStrLn "=== prop_finishUndoesStart" >> HoleyQuickCheck.quickCheckResult prop_finishUndoesStart
      , putStrLn "=== prop_runWithoutActiveCallsDecodes" >> HoleyQuickCheck.quickCheckResult prop_runWithoutActiveCallsDecodes
      , putStrLn "=== prop_toolCallHasPath" >> HoleyQuickCheck.quickCheckResult prop_toolCallHasPath
      ]
  if all HoleyQuickCheck.isSuccess results then HoleyExit.exitSuccess else HoleyExit.exitFailure
