{-# LANGUAGE OverloadedStrings #-}

-- | Runs a requirement through the holey steps.
module Holey (Config (..), runHoley) where

import Agent (Llm, runAgent)
import Control.Concurrent (newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (SomeException, bracket_, displayException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Core
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Edits
import Progress (Progress, readRun, tryUpdate, update)
import Project (Package, findPackages)
import Prompts
import Tools (readOnlyTool)
import Render (renderModule)
import Verify (buildSuite, mentions, propertyReport, runSuite, summarise)
import Workspace

data Config = Config
  { llm :: Llm,
    llmModel :: Model,
    toolDefinitions :: [(ToolName, ToolDefinition)],
    -- | The user's project, where files are read and written.
    projectRoot :: FilePath
  }

data Env = Env
  { config :: Config,
    progress :: Progress,
    packages :: [Package]
  }

runHoley :: Config -> Progress -> IO ()
runHoley cfg prog = do
  result <- try $ do
    dirty <- uncommittedChanges (projectRoot cfg)
    when dirty $
      update prog (\r -> r {warnings = warnings r ++ ["The working tree has uncommitted changes, so this run's edits will be mixed in with them."]})
    found <- findPackages (projectRoot cfg)
    if null found
      then update prog (setStep DesignTypes (Failed "No cabal package with a library was found in the working directory."))
      else do
        outcome <- runExceptT (pipeline (Env cfg prog found))
        either (const (pure ())) pure outcome
  case result of
    Left err -> failCurrent prog ("Internal error: " <> T.pack (displayException (err :: SomeException)))
    Right () -> pure ()

-- | Each stage records its own failure on the run before stopping the pipeline.
pipeline :: Env -> ExceptT () IO ()
pipeline env = do
  req <- liftIO (requirement <$> readRun (progress env))
  let chat = Chat [systemMessage designSystemPrompt] (llmModel (config env)) []
  chat1 <- designStep env DesignTypes SetTypes (typesInstruction req (packages env)) chat
  chat2 <- designStep env DesignSignatures SetFunctions functionsInstruction chat1
  _ <- designStep env WriteProperties SetProperties propertiesInstruction chat2
  _ <- verifyBuild env CheckSkeleton
  functionNames <- liftIO (map fnName . functions . artefact <$> readRun (progress env))
  implementStep env [(name, Nothing) | name <- functionNames]
  _ <- verifyBuild env TypeCheck
  propertiesStep env

-- | Runs one design step in the shared design conversation. The step passes
-- once the model's call to the step's tool is accepted.
designStep :: Env -> Step -> ToolName -> Text -> Chat -> ExceptT () IO Chat
designStep env step tool instruction chat = do
  liftIO (update (progress env) (setStep step Running))
  let prepared = addMessage (userMessage instruction) chat {tools = definitionsFor env [ReadFile, Discover, Search, tool]}
  (done, final) <- liftIO (untilAccepted env 30 tool (designTool env tool) prepared)
  if done
    then liftIO (update (progress env) (setStep step Passed)) >> pure final
    else stepFailed env step ("The model did not call " <> toolNameText tool <> ".")

designTool :: Env -> ToolName -> IO () -> ToolRequest -> IO Text
designTool env tool onAccepted request =
  case readOnlyTool name args of
    Just run -> run
    Nothing
      | name /= tool -> pure (toolNameText name <> " is not available in this step.")
      | otherwise -> do
          result <- case name of
            SetTypes -> either (pure . Left) setTypes (decodeArgs args)
            SetFunctions -> either (pure . Left) (tryUpdate (progress env) . applyFunctions) (decodeArgs args)
            SetProperties -> either (pure . Left) (tryUpdate (progress env) . applyProperties) (decodeArgs args)
            _ -> pure (Left (toolNameText name <> " is not available in this step."))
          case result of
            Left err -> pure ("Rejected: " <> err)
            Right () -> onAccepted >> pure "Accepted."
  where
    function = toolRequestFunction request
    name = toolRequestFunctionParamsName function
    args = toolRequestFunctionParamsArguments function
    setTypes decoded = do
      run <- readRun (progress env)
      case applyTypes (packages env) decoded run of
        Left err -> pure (Left err)
        Right proposed -> case resolveTarget (packages env) proposed of
          Nothing -> pure (Left "Could not resolve the module's location.")
          Just target -> do
            clash <- existingFiles (projectRoot (config env)) proposed [targetModulePath target, targetPropertiesPath target]
            if null clash
              then tryUpdate (progress env) (applyTypes (packages env) decoded)
              else pure (Left ("Already exists: " <> T.intercalate ", " (map T.pack clash) <> ". Choose a new module name."))

-- | Writes the artefact and builds the property suite, which also builds the
-- library. Returns the module's function spans.
verifyBuild :: Env -> Step -> ExceptT () IO ()
verifyBuild env step = do
  liftIO (update (progress env) (setStep step Running))
  run <- liftIO (readRun (progress env))
  target <- maybe (stepFailed env step "No module was chosen.") pure (resolveTarget (packages env) run)
  written <- liftIO (writeArtefact (projectRoot (config env)) target run)
  case written of
    Left err -> stepFailed env step err
    Right (touched, _) -> do
      liftIO (update (progress env) (\r -> r {touchedFiles = touched}))
      built <- liftIO (buildSuite (projectRoot (config env)) (targetSuite target))
      case built of
        Left output -> stepFailed env step (summarise output)
        Right () -> liftIO (update (progress env) (setStep step Passed))

-- | Runs an agent until the handler accepts a call to the tool, reminding the
-- model to call it up to twice. The handler is given the action that records
-- acceptance.
untilAccepted :: Env -> Int -> ToolName -> (IO () -> ToolRequest -> IO Text) -> Chat -> IO (Bool, Chat)
untilAccepted env budget tool handler chat = do
  accepted <- newIORef False
  let attempt :: Int -> Chat -> IO Chat
      attempt remaining c = do
        c' <- runAgent (llm (config env)) budget (handler (writeIORef accepted True)) c
        done <- readIORef accepted
        if done || remaining <= 1
          then pure c'
          else attempt (remaining - 1) (addMessage (userMessage (callToolNudge tool)) c')
  final <- attempt 3 chat
  done <- readIORef accepted
  pure (done, final)

definitionsFor :: Env -> [ToolName] -> [ToolDefinition]
definitionsFor env = mapMaybe (`lookup` toolDefinitions (config env))

-- | Implements the given functions in parallel, each by its own agent that
-- sees only the skeleton, its signature and documentation, the properties
-- that mention it, and any feedback from a failed attempt. Functions not
-- listed keep their current definitions.
implementStep :: Env -> [(FunctionName, Maybe Text)] -> ExceptT () IO ()
implementStep env work = do
  run <- liftIO (readRun (progress env))
  target <- maybe (stepFailed env Implement "No module was chosen.") pure (resolveTarget (packages env) run)
  let art = artefact run
      skeleton = fst (renderModule (targetName target) art {functions = [f {fnBody = Nothing, fnImports = []} | f <- functions art]})
      queued = map fst work
  liftIO $
    update
      (progress env)
      ( \r ->
          (setStep Implement Running r)
            { tasks = [(fnName f, if fnName f `elem` queued then Queued else Done) | f <- functions art]
            }
      )
  slots <- liftIO (newQSem 4)
  liftIO $
    forConcurrently_ work $ \(name, feedback) ->
      bracket_ (waitQSem slots) (signalQSem slots) $
        case [f | f <- functions art, fnName f == name] of
          fn : _ -> implementOne env skeleton fn [p | p <- properties art, mentions (fnName fn) (propSource p)] feedback
          [] -> update (progress env) (setTask name (TaskFailed "Unknown function"))
  after <- liftIO (readRun (progress env))
  case [functionNameText n | (n, TaskFailed _) <- tasks after] of
    [] -> liftIO (update (progress env) (setStep Implement Passed))
    failed -> stepFailed env Implement ("Could not implement " <> T.intercalate ", " failed <> ".")

implementOne :: Env -> Text -> FunctionSpec -> [PropertySpec] -> Maybe Text -> IO ()
implementOne env skeleton fn props feedback = do
  update (progress env) (setTask (fnName fn) Working)
  let chat =
        Chat
          [ userMessage (implementInstruction skeleton fn props feedback),
            systemMessage implementSystemPrompt
          ]
          (llmModel (config env))
          (definitionsFor env [ImplementFunction])
      handler onAccepted request
        | toolRequestFunctionParamsName (toolRequestFunction request) /= ImplementFunction =
            pure "Only ImplementFunction is available."
        | otherwise = do
            result <- case decodeArgs (toolRequestFunctionParamsArguments (toolRequestFunction request)) of
              Left err -> pure (Left err)
              Right args -> tryUpdate (progress env) (applyImplementation (fnName fn) args)
            case result of
              Left err -> pure ("Rejected: " <> err)
              Right () -> onAccepted >> pure "Accepted."
  outcome <- try (untilAccepted env 6 ImplementFunction handler chat)
  update (progress env) $ setTask (fnName fn) $ case outcome of
    Right (True, _) -> Done
    Right (False, _) -> TaskFailed "The model did not submit an implementation."
    Left err -> TaskFailed (T.pack (displayException (err :: SomeException)))

-- | Runs the property suite.
propertiesStep :: Env -> ExceptT () IO ()
propertiesStep env = do
  liftIO (update (progress env) (setStep RunProperties Running))
  run <- liftIO (readRun (progress env))
  target <- maybe (stepFailed env RunProperties "No module was chosen.") pure (resolveTarget (packages env) run)
  result <- liftIO (runSuite (projectRoot (config env)) (targetSuite target))
  case result of
    Right _ -> liftIO (update (progress env) (setStep RunProperties Passed))
    Left output -> stepFailed env RunProperties (propertyReport output)

stepFailed :: Env -> Step -> Text -> ExceptT () IO a
stepFailed env step reason = do
  liftIO (update (progress env) (setStep step (Failed reason)))
  throwE ()

-- | Marks the running step (or the first step) as failed.
failCurrent :: Progress -> Text -> IO ()
failCurrent prog reason = do
  run <- readRun prog
  let step = case [s | (s, Running) <- steps run] of
        s : _ -> s
        [] -> DesignTypes
  update prog (setStep step (Failed reason))

systemMessage :: Text -> Message
systemMessage text = Message System (Just text) [] Nothing Nothing

userMessage :: Text -> Message
userMessage text = Message User (Just text) [] Nothing Nothing
