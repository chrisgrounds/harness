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
    ComponentName (..),
    FunctionRef (..),
    FunctionKind (..),
    Placeholder (..),
    FunctionSpec (..),
    fnName,
    isInstanceName,
    PropertySpec (..),
    ModulePlan (..),
    Artefact (..),
    mainModule,
    Run (..),
    touchedPaths,
    allSteps,
    stepLabel,
    emptyArtefact,
    newRun,
    addHistory,
    addWarning,
    overArtefact,
    overFunctions,
    setStep,
    setTask,
    restartFrom,
    currentStep,
    taskProgress,
    startToolCall,
    finishToolCall,
  )
where

import Data.Aeson (FromJSON (..), ToJSON, Value (..), defaultOptions, genericParseJSON, withObject)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (delete)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Core.ChangedFile (ChangedFile (..))
import Core.Model (Model)
import Core.Tool (ToolCall, ToolRequest, toolCallOf)
import Core.RunTime
import qualified Data.Aeson as Aeson

newtype Requirement = Requirement {requirementText :: Text}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data Step
  = Discover
  | DesignTypes
  | DesignSignatures
  | WriteProperties
  | CheckSkeleton
  | Implement
  | TypeCheck
  | RunProperties
  | Review
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, ToJSON, FromJSON)

data StepStatus = Pending | Running | Passed | Failed Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data TaskStatus = Queued | Working | Done | TaskFailed Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype FunctionName = FunctionName {functionNameText :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype ModuleName = ModuleName {moduleNameText :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

-- | A cabal target, such as @demo@ for a library or @demo:exe:app@.
newtype ComponentName = ComponentName {componentNameText :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

-- | Whether a repaired declaration is an instance, named like
-- @instance Show Foo@, rather than a function.
isInstanceName :: FunctionName -> Bool
isInstanceName (FunctionName name) = "instance " `T.isPrefixOf` name

-- | A function by the file it is defined in, since names repeat across
-- modules (every executable has a @main@).
data FunctionRef = FunctionRef
  { -- | Relative to the project root.
    refPath :: FilePath,
    refName :: FunctionName
  }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data FunctionKind
  = -- | Designed in step 2: a new function, or a replacement for the existing
    -- function of the same name.
    Designed
  | -- | An existing function that stopped type checking because the design
    -- changed. It keeps its signature and documentation; only its equations
    -- are rewritten.
    Repair
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | What stands in for a function's body until it is implemented.
data Placeholder
  = -- | The previous definition, with a wildcard for each new parameter.
    PreviousBody
  | Undefined
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FunctionSpec = FunctionSpec
  { fnRef :: FunctionRef,
    fnKind :: FunctionKind,
    -- | The type, without the name or @::@.
    fnSignature :: Text,
    -- | What the function does, for agent context only; never written into source.
    fnDoc :: Text,
    -- | For a function that already existed, what to change, for its
    -- implementer only.
    fnChange :: Text,
    -- | The complete definition (every equation, no signature). 'Nothing'
    -- is rendered as the placeholder.
    fnBody :: Maybe Text,
    fnPlaceholder :: Placeholder,
    -- | Extra import lines the implementation needs.
    fnImports :: [Text],
    -- | The declaration as it was before the run, for functions that already
    -- existed.
    fnPrevious :: Maybe Text,
    -- | The model the design chose to implement it, if it chose one.
    fnModel :: Maybe Model
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

fnName :: FunctionSpec -> FunctionName
fnName = refName . fnRef

data PropertySpec = PropertySpec
  { propName :: Text,
    propSource :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | A module the run creates or changes.
data ModulePlan = ModulePlan
  { -- | The cabal target the module belongs to, e.g. @demo@ or @demo:exe:app@.
    planComponent :: ComponentName,
    planModule :: ModuleName,
    -- | Relative to the project root.
    planPath :: FilePath,
    -- | Whether the run creates the module, rather than changing an existing one.
    planIsNew :: Bool,
    -- | LANGUAGE extensions to add.
    planExtensions :: [Text],
    -- | Import lines to add.
    planImports :: [Text],
    -- | Type declarations and instances, without imports or a module header.
    -- In an existing module each replaces the declaration of the same name,
    -- or is added after the others.
    planTypes :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Artefact = Artefact
  { -- | The first is the main module, which the property suite is named after
    -- and built against.
    modules :: [ModulePlan],
    -- | Designed functions, then repairs, in any of the modules or elsewhere.
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
    iteration :: Int,
    steps :: [(Step, StepStatus)],
    tasks :: [(FunctionRef, TaskStatus)],
    artefact :: Artefact,
    touchedFiles :: [ChangedFile],
    warnings :: [Text],
    history :: [Text],
    activeToolCalls :: [ToolCall],
    runTiming :: RunTiming
  }
  deriving (Eq, Show, Generic, ToJSON)

-- | Runs saved before 'activeToolCalls' existed decode with none running.
instance FromJSON Run where
  parseJSON = withObject "Run" $ \o ->
    genericParseJSON defaultOptions (Object (KeyMap.insertWith (\_ saved -> saved) "runTiming" (Aeson.toJSON TimingUnknown) (KeyMap.insertWith (\_ saved -> saved) "activeToolCalls" (Array mempty) o)))

touchedPaths :: Run -> [FilePath]
touchedPaths = map changedPath . touchedFiles

allSteps :: [Step]
allSteps = [minBound .. maxBound]

stepLabel :: Step -> Text
stepLabel Discover = "Discovering and planning"
stepLabel DesignTypes = "Creating types"
stepLabel DesignSignatures = "Creating functions"
stepLabel WriteProperties = "Creating properties"
stepLabel CheckSkeleton = "Checking skeleton"
stepLabel Implement = "Implementing functions"
stepLabel TypeCheck = "Type checking"
stepLabel RunProperties = "Running properties"
stepLabel Review = "Reviewing against the requirement"

emptyArtefact :: Artefact
emptyArtefact = Artefact [] [] [] "" []

mainModule :: Artefact -> Maybe ModulePlan
mainModule art = case modules art of
  first : _ -> Just first
  [] -> Nothing

newRun :: Requirement -> Run
newRun req =
  Run
    { requirement = req,
      iteration = 1,
      steps = [(s, Pending) | s <- allSteps],
      tasks = [],
      artefact = emptyArtefact,
      touchedFiles = [],
      warnings = [],
      history = [],
      activeToolCalls = [],
      runTiming = TimingUnknown
    }

addHistory :: Text -> Run -> Run
addHistory text run = run {history = history run ++ [text]}

addWarning :: Text -> Run -> Run
addWarning text run = run {warnings = warnings run ++ [text]}

overArtefact :: (Artefact -> Artefact) -> Run -> Run
overArtefact f run = run {artefact = f (artefact run)}

overFunctions :: ([FunctionSpec] -> [FunctionSpec]) -> Run -> Run
overFunctions f = overArtefact (\art -> art {functions = f (functions art)})

setStep :: Step -> StepStatus -> Run -> Run
setStep step status run =
  run {steps = [(s, if s == step then status else current) | (s, current) <- steps run]}

-- | Updates a task's status, appending the task if it is not yet known.
setTask :: FunctionRef -> TaskStatus -> Run -> Run
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

-- | Records a tool request the model made as running, for the client's
-- status bar.
startToolCall :: ToolRequest -> Run -> Run
startToolCall t run = run {activeToolCalls = activeToolCalls run ++ [toolCallOf t]}

-- | Removes one running call matching the request, once it has finished.
finishToolCall :: ToolRequest -> Run -> Run
finishToolCall t run = run {activeToolCalls = delete (toolCallOf t) (activeToolCalls run)}
