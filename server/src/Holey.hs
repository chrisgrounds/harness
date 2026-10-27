{-# LANGUAGE OverloadedStrings #-}

-- | Runs a requirement through the holey steps.
module Holey (Config (..), runHoley) where

import Agent (Llm, runAgent)
import Control.Exception (SomeException, displayException, try)
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
import Verify (buildSuite, summarise)
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
  pure ()

-- | Runs one design step in the shared design conversation. The step passes
-- once the model's call to the step's tool is accepted.
designStep :: Env -> Step -> ToolName -> Text -> Chat -> ExceptT () IO Chat
designStep env step tool instruction chat = do
  liftIO (update (progress env) (setStep step Running))
  accepted <- liftIO (newIORef False)
  let available = [ReadFile, Discover, Search, tool]
      prepared = addMessage (userMessage instruction) chat {tools = mapMaybe (`lookup` toolDefinitions (config env)) available}
      handler = designTool env tool (writeIORef accepted True)
      attempt :: Int -> Chat -> IO Chat
      attempt remaining c = do
        c' <- runAgent (llm (config env)) 30 handler c
        done <- readIORef accepted
        if done || remaining <= 1
          then pure c'
          else attempt (remaining - 1) (addMessage (userMessage (callToolNudge tool)) c')
  final <- liftIO (attempt 3 prepared)
  done <- liftIO (readIORef accepted)
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
