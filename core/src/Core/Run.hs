{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Run
  ( Requirement (..),
    Step (..),
    StepStatus (..),
    TaskStatus (..),
    FunctionName (..),
    ModuleName (..),
    FunctionSpec (..),
    PropertySpec (..),
    Artefact (..),
    Run (..),
    allSteps,
    stepLabel,
    emptyArtefact,
    newRun,
    setStep,
    setTask,
    restartFrom,
    currentStep,
    taskProgress,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Requirement = Requirement {requirementText :: Text}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data Step
  = DesignTypes
  | DesignSignatures
  | WriteProperties
  | CheckSkeleton
  | Implement
  | TypeCheck
  | RunProperties
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, ToJSON, FromJSON)

data StepStatus = Pending | Running | Passed | Failed Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data TaskStatus = Queued | Working | Done | TaskFailed Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype FunctionName = FunctionName {functionNameText :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype ModuleName = ModuleName {moduleNameText :: Text}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data FunctionSpec = FunctionSpec
  { fnName :: FunctionName,
    fnSignature :: Text,
    fnDoc :: Text,
    -- | The complete definition (every equation, no signature). 'Nothing' is
    -- rendered as @undefined@.
    fnBody :: Maybe Text,
    -- | Extra import lines the implementation needs.
    fnImports :: [Text]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PropertySpec = PropertySpec
  { propName :: Text,
    propSource :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Artefact = Artefact
  { extensions :: [Text],
    -- | Import lines for the new module.
    imports :: [Text],
    -- | Type declarations and instances, without imports or a module header.
    typesSource :: Text,
    functions :: [FunctionSpec],
    -- | Import lines for the property module, beyond the new module and QuickCheck.
    propertyImports :: [Text],
    -- | Arbitrary instances and generators used by the properties.
    propertyHelpers :: Text,
    properties :: [PropertySpec]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Run = Run
  { requirement :: Requirement,
    -- | Starts at 1 and increases each time a failed verification loops back.
    iteration :: Int,
    steps :: [(Step, StepStatus)],
    -- | One entry per function being implemented, in declaration order.
    tasks :: [(FunctionName, TaskStatus)],
    artefact :: Artefact,
    -- | The cabal target the module belongs to, e.g. @demo@ or @demo:exe:app@.
    targetComponent :: Maybe Text,
    targetModule :: Maybe ModuleName,
    -- | Files the run created or edited, relative to the working directory.
    touchedFiles :: [FilePath],
    warnings :: [Text],
    -- | Why the run looped back or gave up, oldest first.
    history :: [Text]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

allSteps :: [Step]
allSteps = [minBound .. maxBound]

stepLabel :: Step -> Text
stepLabel DesignTypes = "Creating types"
stepLabel DesignSignatures = "Creating functions"
stepLabel WriteProperties = "Creating properties"
stepLabel CheckSkeleton = "Checking skeleton"
stepLabel Implement = "Implementing functions"
stepLabel TypeCheck = "Type checking"
stepLabel RunProperties = "Running properties"

emptyArtefact :: Artefact
emptyArtefact = Artefact [] [] "" [] [] "" []

newRun :: Requirement -> Run
newRun req =
  Run
    { requirement = req,
      iteration = 1,
      steps = [(s, Pending) | s <- allSteps],
      tasks = [],
      artefact = emptyArtefact,
      targetComponent = Nothing,
      targetModule = Nothing,
      touchedFiles = [],
      warnings = [],
      history = []
    }

setStep :: Step -> StepStatus -> Run -> Run
setStep step status run =
  run {steps = [(s, if s == step then status else current) | (s, current) <- steps run]}

-- | Updates a task's status, appending the task if it is not yet known.
setTask :: FunctionName -> TaskStatus -> Run -> Run
setTask name status run
  | any ((== name) . fst) (tasks run) =
      run {tasks = [(n, if n == name then status else current) | (n, current) <- tasks run]}
  | otherwise = run {tasks = tasks run ++ [(name, status)]}

-- | Starts a new iteration: the given step and every later step return to 'Pending'.
restartFrom :: Step -> Run -> Run
restartFrom step run =
  run
    { iteration = iteration run + 1,
      steps = [(s, if s >= step then Pending else current) | (s, current) <- steps run],
      tasks = if step <= Implement then [] else tasks run
    }

-- | The step currently running, or the step that failed if the run gave up.
currentStep :: Run -> Maybe (Step, StepStatus)
currentStep run = case filter (active . snd) (steps run) of
  found : _ -> Just found
  [] -> Nothing
  where
    active Running = True
    active (Failed _) = True
    active _ = False

-- | Finished and total task counts.
taskProgress :: Run -> (Int, Int)
taskProgress run = (length [() | (_, Done) <- tasks run], length (tasks run))
