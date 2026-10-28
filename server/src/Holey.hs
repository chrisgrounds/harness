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
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Edits
import Progress (Progress, readRun, tryUpdate, update)
import Project (Component, findComponents)
import Prompts
import Tools (readOnlyTool)
import Policy
import Render (Span, renderModule)
import System.Directory (doesDirectoryExist)
import System.FilePath (isAbsolute)
import Verify
import Workspace

data Config = Config
  { llm :: Llm,
    llmModel :: Model,
    toolDefinitions :: [(ToolName, ToolDefinition)]
  }

data Env = Env
  { config :: Config,
    -- | The user's project, where files are read and written.
    projectRoot :: FilePath,
    progress :: Progress,
    components :: [Component]
  }

runHoley :: Config -> FilePath -> Progress -> IO ()
runHoley cfg root prog = do
  result <- try $ do
    exists <- doesDirectoryExist root
    if not (isAbsolute root && exists)
      then update prog (setStep DesignTypes (Failed ("The project directory " <> T.pack root <> " is not an absolute path to an existing directory.")))
      else do
        dirty <- uncommittedChanges root
        when dirty $
          update prog (\r -> r {warnings = warnings r ++ ["The working tree has uncommitted changes, so this run's edits will be mixed in with them."]})
        found <- findComponents root
        if null found
          then update prog (setStep DesignTypes (Failed ("No cabal library or executable was found in " <> T.pack root <> ".")))
          else do
            outcome <- runExceptT (pipeline (Env cfg root prog found))
            either (const (pure ())) pure outcome
  case result of
    Left err -> failCurrent prog ("Internal error: " <> T.pack (displayException (err :: SomeException)))
    Right () -> pure ()

-- | Each stage records its result on the run. Failed verifications loop back
-- as 'decide' says; any other failure stops the run.
pipeline :: Env -> ExceptT () IO ()
pipeline env = do
  req <- liftIO (requirement <$> readRun (progress env))
  let typesStep = typesInstruction req (components env)
  design typesStep (Chat [systemMessage designSystemPrompt] (llmModel (config env)) [])
  where
    design :: Text -> Chat -> ExceptT () IO ()
    design typesStep chat = do
      chat1 <- designStep env DesignTypes SetTypes typesStep chat
      chat2 <- designStep env DesignSignatures SetFunctions functionsInstruction chat1
      chat3 <- designStep env WriteProperties SetProperties propertiesInstruction chat2
      skeleton <- verifyBuild env CheckSkeleton
      case skeleton of
        Left (output, _) -> loopBack typesStep chat3 [] (SkeletonFailed (summarise output))
        Right () -> do
          names <- liftIO (functionNames env)
          build typesStep chat3 [] [(name, Nothing) | name <- names]

    build :: Text -> Chat -> [Text] -> [(FunctionName, Maybe Text)] -> ExceptT () IO ()
    build typesStep chat previouslyFailed work = do
      missing <- implementStep env work
      if not (null missing)
        then loopBack typesStep chat previouslyFailed (ImplementationMissing missing)
        else do
          typed <- verifyBuild env TypeCheck
          case typed of
            Left (output, spans) -> do
              file <- liftIO (maybe "" targetModulePath . resolveTarget (components env) <$> readRun (progress env))
              let (attributed, unattributed) = attributeErrors file spans (ghcErrors output)
                  rest = if null attributed && null unattributed then [summarise output] else unattributed
              loopBack typesStep chat previouslyFailed (TypeErrors attributed rest)
            Right () -> do
              checked <- propertiesStep env
              case checked of
                Right () -> pure ()
                Left output -> do
                  art <- liftIO (artefact <$> readRun (progress env))
                  let failed = failedProperties output
                      suspects =
                        nub
                          [ fnName f
                          | p <- properties art,
                            propName p `elem` failed,
                            f <- functions art,
                            mentions (fnName f) (propSource p)
                          ]
                  loopBack typesStep chat previouslyFailed (PropertiesFailed failed suspects (propertyReport output))

    loopBack :: Text -> Chat -> [Text] -> Outcome -> ExceptT () IO ()
    loopBack typesStep chat previouslyFailed outcome = do
      run <- liftIO (readRun (progress env))
      names <- liftIO (functionNames env)
      let current = iteration run
          record text = liftIO (update (progress env) (\r -> r {history = history r ++ [text]}))
          failedNow = case outcome of
            PropertiesFailed failed _ _ -> failed
            _ -> []
      case decide current names previouslyFailed outcome of
        GiveUp reason -> record reason >> throwE ()
        Redesign feedback -> do
          record ("Iteration " <> T.pack (show current) <> ": " <> describeOutcome outcome <> ", so redesigning.")
          liftIO (update (progress env) (restartFrom DesignTypes))
          design typesStep (addMessage (userMessage (revisionNote (current + 1) feedback)) chat)
        Reimplement work -> do
          record
            ( "Iteration "
                <> T.pack (show current)
                <> ": "
                <> describeOutcome outcome
                <> ", so reimplementing "
                <> T.intercalate ", " (map (functionNameText . fst) work)
                <> "."
            )
          liftIO (update (progress env) (restartFrom Implement))
          build typesStep chat failedNow work

functionNames :: Env -> IO [FunctionName]
functionNames env = map fnName . functions . artefact <$> readRun (progress env)

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
  case readOnlyTool (projectRoot env) name args of
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
      case applyTypes (components env) decoded run of
        Left err -> pure (Left err)
        Right proposed -> case resolveTarget (components env) proposed of
          Nothing -> pure (Left "Could not resolve the module's location.")
          Just target -> do
            clash <- existingFiles (projectRoot env) proposed [targetModulePath target, targetPropertiesPath target]
            if null clash
              then tryUpdate (progress env) (applyTypes (components env) decoded)
              else pure (Left ("Already exists: " <> T.intercalate ", " (map T.pack clash) <> ". Choose a new module name."))

-- | Writes the artefact and builds the property suite, which also builds the
-- library. A failed build returns its output and where each function is.
verifyBuild :: Env -> Step -> ExceptT () IO (Either (Text, [(FunctionName, Span)]) ())
verifyBuild env step = do
  liftIO (update (progress env) (setStep step Running))
  run <- liftIO (readRun (progress env))
  target <- maybe (stepFailed env step "No module was chosen.") pure (resolveTarget (components env) run)
  written <- liftIO (writeArtefact (projectRoot env) target run)
  case written of
    Left err -> stepFailed env step err
    Right (touched, spans) -> liftIO $ do
      update (progress env) (\r -> r {touchedFiles = touched})
      built <- buildSuite (projectRoot env) (targetSuite target)
      case built of
        Left output -> do
          update (progress env) (setStep step (Failed (brief (summarise output))))
          pure (Left (output, spans))
        Right () -> update (progress env) (setStep step Passed) >> pure (Right ())

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
-- listed keep their current definitions. Returns the functions whose agents
-- did not submit an implementation.
implementStep :: Env -> [(FunctionName, Maybe Text)] -> ExceptT () IO [FunctionName]
implementStep env work = do
  run <- liftIO (readRun (progress env))
  target <- maybe (stepFailed env Implement "No module was chosen.") pure (resolveTarget (components env) run)
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
  let missing = [n | (n, TaskFailed _) <- tasks after]
  liftIO $ update (progress env) $ setStep Implement $
    if null missing
      then Passed
      else Failed ("No implementation for " <> T.intercalate ", " (map functionNameText missing) <> ".")
  pure missing

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

-- | Runs the property suite. A failed run returns its output.
propertiesStep :: Env -> ExceptT () IO (Either Text ())
propertiesStep env = do
  liftIO (update (progress env) (setStep RunProperties Running))
  run <- liftIO (readRun (progress env))
  target <- maybe (stepFailed env RunProperties "No module was chosen.") pure (resolveTarget (components env) run)
  result <- liftIO (runSuite (projectRoot env) (targetSuite target))
  liftIO $ case result of
    Right _ -> update (progress env) (setStep RunProperties Passed) >> pure (Right ())
    Left output -> do
      update (progress env) (setStep RunProperties (Failed (brief (propertyReport output))))
      pure (Left output)

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
