{-# LANGUAGE OverloadedStrings #-}

-- | Runs a requirement through the holey steps.
module Holey (Config (..), OnFailure (..), runHoley) where

import Agent (Llm, runAgent)
import Control.Concurrent (newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (SomeException, bracket_, displayException, try)
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Core
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Decls (Decl (..), DeclKind (..), SourceFile (..), findDecl, splitSource)
import Edits
import Policy
import Progress (Progress, readRun, tryUpdate, update)
import Project
import Prompts
import Render (designSummary, renderFile)
import System.Directory (doesDirectoryExist)
import System.FilePath (isAbsolute, takeExtension, (</>))
import Tools (readOnlyTool)
import Verify
import Workspace

data Config = Config
  { llm :: Llm,
    llmModel :: Model,
    toolDefinitions :: [(ToolName, ToolDefinition)],
    onFailure :: OnFailure
  }

-- | What happens to the changes of a run that does not finish.
data OnFailure
  = Keep
  | Revert
  deriving (Eq, Show)

data Env = Env
  { config :: Config,
    -- | The user's project, where files are read and written.
    projectRoot :: FilePath,
    progress :: Progress,
    project :: ProjectInfo,
    originals :: Originals
  }

-- | Runs the requirement. A run that does not finish, because a step failed,
-- it gave up or something went wrong, keeps or reverts its changes as
-- 'onFailure' says.
runHoley :: Config -> FilePath -> Progress -> IO ()
runHoley cfg root prog = do
  origs <- newOriginals
  result <- try $ do
    exists <- doesDirectoryExist root
    if not (isAbsolute root && exists)
      then update prog (setStep DesignTypes (Failed ("The project directory " <> T.pack root <> " is not an absolute path to an existing directory."))) >> pure False
      else do
        dirty <- uncommittedChanges root
        when dirty $
          update prog (\r -> r {warnings = warnings r ++ ["The working tree has uncommitted changes, so this run's edits will be mixed in with them."]})
        info <- findProject root
        if null (components info)
          then update prog (setStep DesignTypes (Failed ("No cabal library or executable was found in " <> T.pack root <> "."))) >> pure False
          else either (const False) (const True) <$> runExceptT (pipeline (Env cfg root prog info origs))
  finished <- case result of
    Left err -> failCurrent prog ("Internal error: " <> T.pack (displayException (err :: SomeException))) >> pure False
    Right done -> pure done
  unless (finished || onFailure cfg == Keep) (undo origs)
  where
    undo origs = do
      run <- readRun prog
      undone <- try (rollback root origs run)
      update prog $ \r -> case undone of
        Right [] -> r
        Right paths -> r {touchedFiles = [], history = history r ++ ["The run did not finish, so its changes to " <> T.pack (show (length paths)) <> " files were undone."]}
        Left err -> r {warnings = warnings r ++ ["Could not undo the run's changes: " <> T.pack (displayException (err :: SomeException))]}

-- | Each stage records its result on the run. Failed verifications loop back
-- as 'decide' says; any other failure stops the run.
pipeline :: Env -> ExceptT () IO ()
pipeline env = do
  req <- liftIO (requirement <$> readRun (progress env))
  let typesStep = typesInstruction req (components (project env))
  design typesStep (Chat [systemMessage designSystemPrompt] (llmModel (config env)) [])
  where
    design :: Text -> Chat -> ExceptT () IO ()
    design typesStep chat = do
      chat1 <- designStep env DesignTypes SetTypes typesStep chat
      chat2 <- designStep env DesignSignatures SetFunctions functionsInstruction chat1
      chat3 <- designStep env WriteProperties SetProperties propertiesInstruction chat2
      verify typesStep chat3 maxFixUps

    -- Errors in what one design step wrote go back to that step, up to
    -- 'maxFixUps' times; anything else, or more, is a redesign.
    verify :: Text -> Chat -> Int -> ExceptT () IO ()
    verify typesStep chat fixUps = do
      skeleton <- checkSkeleton env
      case skeleton of
        Left (feedback, Just tool) | fixUps > 0 -> do
          current <- liftIO (iteration <$> readRun (progress env))
          liftIO . note env $
            "Iteration " <> T.pack (show current) <> ": " <> describeOutcome (SkeletonFailed feedback) <> ": "
              <> redesignReason (SkeletonFailed feedback) <> "; asking " <> toolNameText tool <> " to fix it."
          fixed <- designStep env (stepOf tool) tool (fixUpInstruction tool feedback) chat
          verify typesStep fixed (fixUps - 1)
        Left (feedback, _) -> loopBack typesStep chat [] (SkeletonFailed feedback)
        Right () -> do
          refs <- liftIO (functionRefs env)
          build typesStep chat [] [(ref, Nothing) | ref <- refs]
    stepOf SetTypes = DesignTypes
    stepOf SetFunctions = DesignSignatures
    stepOf _ = WriteProperties

    build :: Text -> Chat -> [Text] -> [(FunctionRef, Maybe Text)] -> ExceptT () IO ()
    build typesStep chat previouslyFailed work = do
      missing <- implementStep env work
      if not (null missing)
        then loopBack typesStep chat previouslyFailed (ImplementationMissing missing)
        else do
          typed <- typeCheck env
          case typed of
            Left (attributed, rest) -> loopBack typesStep chat previouslyFailed (TypeErrors attributed rest)
            Right () -> do
              checked <- propertiesStep env
              case checked of
                Left output -> do
                  art <- liftIO (artefact <$> readRun (progress env))
                  let failed = failedProperties output
                      suspects =
                        nub
                          [ fnRef f
                          | p <- properties art,
                            propName p `elem` failed,
                            f <- functions art,
                            mentions (fnName f) (propSource p)
                          ]
                  loopBack typesStep chat previouslyFailed (PropertiesFailed failed suspects (propertyReport output))
                Right () -> pure ()

    loopBack :: Text -> Chat -> [Text] -> Outcome -> ExceptT () IO ()
    loopBack typesStep chat previouslyFailed outcome = do
      run <- liftIO (readRun (progress env))
      refs <- liftIO (functionRefs env)
      let current = iteration run
          record text = liftIO (update (progress env) (\r -> r {history = history r ++ [text]}))
          failedNow = case outcome of
            PropertiesFailed failed _ _ -> failed
            _ -> []
      case decide current refs previouslyFailed outcome of
        GiveUp reason -> record reason >> throwE ()
        Redesign feedback -> do
          record ("Iteration " <> T.pack (show current) <> ": " <> describeOutcome outcome <> ": " <> redesignReason outcome)
          liftIO (update (progress env) (restartFrom DesignTypes))
          design typesStep (addMessage (userMessage (revisionNote (current + 1) feedback)) chat)
        Reimplement work -> do
          record
            ( "Iteration "
                <> T.pack (show current)
                <> ": "
                <> describeOutcome outcome
                <> ", so reimplementing "
                <> T.intercalate ", " (map (functionNameText . refName . fst) work)
                <> "."
            )
          liftIO (update (progress env) (restartFrom Implement))
          build typesStep chat failedNow work

-- | Skeleton errors sent back to a design step before the run redesigns.
maxFixUps :: Int
maxFixUps = 3

-- | Adds a line to the run's history.
note :: Env -> Text -> IO ()
note env text = update (progress env) (\r -> r {history = history r ++ [text]})

functionRefs :: Env -> IO [FunctionRef]
functionRefs env = map fnRef . functions . artefact <$> readRun (progress env)

-- | Runs one design step in the shared design conversation. The step passes
-- once the model's call to the step's tool is accepted. Every step offers
-- the same tools, and 'designTool' refuses the other steps' ones, so the
-- conversation only ever grows: Claude's thinking is bound to what came
-- before it, and an unchanged prefix stays cached.
designStep :: Env -> Step -> ToolName -> Text -> Chat -> ExceptT () IO Chat
designStep env step tool instruction chat = do
  liftIO (update (progress env) (setStep step Running))
  let prepared = addMessage (userMessage instruction) chat {tools = definitionsFor env [ReadFile, Discover, Search, SetTypes, SetFunctions, SetProperties]}
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
            SetTypes -> either (pure . Left) (setTypes env) (decodeArgs args)
            SetFunctions -> either (pure . Left) (setFunctions env) (decodeArgs args)
            SetProperties -> either (pure . Left) (tryUpdate (progress env) . applyProperties) (decodeArgs args)
            _ -> pure (Left (toolNameText name <> " is not available in this step."))
          case result of
            Left err -> pure ("Rejected: " <> err)
            Right () -> onAccepted >> pure "Accepted."
  where
    function = toolRequestFunction request
    name = toolRequestFunctionParamsName function
    args = toolRequestFunctionParamsArguments function

-- | Finds each module on disk: a module that exists is changed, any other is
-- created. Existing modules are read now, before the run writes anything.
setTypes :: Env -> TypesArgs -> IO (Either Text ())
setTypes env args = case checkModules (components (project env)) args of
  Left err -> pure (Left err)
  Right checked -> do
    plans <- forM checked $ \(c, name, arg) -> do
      located <- locateModule (projectRoot env) c name
      before <- maybe (pure Nothing) (original (projectRoot env) (originals env)) located
      pure
        ModulePlan
          { planComponent = componentName c,
            planModule = name,
            planPath = fromMaybe (modulePath c name) located,
            -- A module this run created earlier is still new.
            planIsNew = isNothing before,
            planExtensions = argExtensions arg,
            planImports = argImports arg,
            planTypes = argTypes arg
          }
    run <- readRun (progress env)
    clash <- case suiteOf (components (project env)) (artefact run) {modules = plans} of
      Just suite -> existingFiles (projectRoot env) run [suiteFile suite]
      Nothing -> pure []
    case [p | p <- plans, planIsNew p, planModule p == ModuleName "Main"] of
      p : _ -> pure (Left ("A new module cannot be called Main: " <> planComponent p <> " already has its Main. Choose another name."))
      []
        | null clash -> tryUpdate (progress env) (applyModules plans)
        | otherwise -> pure (Left ("Already exists: " <> T.intercalate ", " (map T.pack clash) <> ". List another module first."))

setFunctions :: Env -> FunctionsArgs -> IO (Either Text ())
setFunctions env args = do
  plans <- modules . artefact <$> readRun (progress env)
  previous <- fmap concat . forM [p | p <- plans, not (planIsNew p)] $ \p -> do
    before <- original (projectRoot env) (originals env) (planPath p)
    pure
      [ (FunctionRef (planPath p) (FunctionName key), declText d)
      | Just text <- [before],
        d <- sourceDecls (splitSource text),
        declKind d == ValueDecl,
        Just key <- [declKey d]
      ]
  tryUpdate (progress env) (applyFunctions (`Map.lookup` Map.fromList previous) args)

-- | Builds the skeleton. Existing functions and instances it breaks become
-- holes to repair, a changed function whose old equations do not fit its
-- new signature becomes @undefined@, and it is built again. Other errors
-- are returned with the design step that wrote the code they are in, if
-- one did.
checkSkeleton :: Env -> ExceptT () IO (Either (Text, Maybe ToolName) ())
checkSkeleton env = do
  liftIO (update (progress env) (setStep CheckSkeleton Running))
  attempt (1 :: Int)
  where
    attempt rounds = do
      built <- writeAndBuild env CheckSkeleton
      case built of
        Right () -> liftIO (update (progress env) (setStep CheckSkeleton Passed)) >> pure (Right ())
        Left output -> do
          blamed <- liftIO (blameErrors env output)
          repairs <- liftIO (repairRefs env)
          kept <- liftIO (keptBodies env)
          run <- liftIO (readRun (progress env))
          let others = nub [ref | (_, _, InOther ref) <- blamed]
              planned = nub [ref | (_, _, InPlanned ref) <- blamed]
              unfit = filter (`elem` kept) planned
          if (not (null others) || not (null unfit)) && rounds < 8
            then do
              liftIO (previousDecls env others >>= update (progress env) . addRepairs)
              liftIO (update (progress env) (withUndefined unfit))
              attempt (rounds + 1)
            else do
              let stuck = filter (`elem` repairs) planned
                  feedback = (if null stuck then "" else brokenSignaturesNote stuck <> "\n") <> summarise output
              liftIO (update (progress env) (setStep CheckSkeleton (Failed (brief (summarise output)))))
              pure (Left (feedback, owningStep (components (project env)) (artefact run) blamed))

-- | Builds the implementation. Errors are blamed on the functions they are
-- in; existing functions that break become repairs.
typeCheck :: Env -> ExceptT () IO (Either ([(FunctionRef, Text)], [Text]) ())
typeCheck env = do
  liftIO (update (progress env) (setStep TypeCheck Running))
  built <- writeAndBuild env TypeCheck
  case built of
    Right () -> liftIO (update (progress env) (setStep TypeCheck Passed)) >> pure (Right ())
    Left output -> liftIO $ do
      blamed <- blameErrors env output
      previousDecls env (nub [ref | (_, _, InOther ref) <- blamed]) >>= update (progress env) . addRepairs
      let owner (InPlanned ref) = Just ref
          owner (InOther ref) = Just ref
          owner Unblamed = Nothing
          attributed = [(ref, T.unlines [errorText e | (e, _, b) <- blamed, owner b == Just ref]) | ref <- nub (mapMaybe (\(_, _, b) -> owner b) blamed)]
          unblamed = [errorText e | (e, _, Unblamed) <- blamed]
          rest = if null attributed && null unblamed then [summarise output] else unblamed
      update (progress env) (setStep TypeCheck (Failed (brief (summarise output))))
      pure (Left (attributed, rest))

-- | Writes the artefact and builds every package with its test suites.
writeAndBuild :: Env -> Step -> ExceptT () IO (Either Text ())
writeAndBuild env step = do
  run <- liftIO (readRun (progress env))
  written <- liftIO (writeArtefact (projectRoot env) (components (project env)) (originals env) run)
  case written of
    Left err -> stepFailed env step err
    Right touched -> liftIO $ do
      update (progress env) (\r -> r {touchedFiles = touched})
      buildAll (projectRoot env)

-- | Blames each error in build output on where it is in the files as
-- written, with the file it is in. An instance the run declared in step 1
-- is the design's, not one to repair.
blameErrors :: Env -> Text -> IO [(GhcError, Maybe FilePath, Blame)]
blameErrors env output = do
  run <- readRun (progress env)
  let art = artefact run
      fns = functions art
      suite = suiteFile <$> suiteOf (components (project env)) art
      declared p = [key | plan <- modules art, planPath plan == p, d <- sourceDecls (splitSource (planTypes plan)), Just key <- [declKey d]]
  forM (ghcErrors output) $ \e -> do
    path <- resolveErrorFile (projectRoot env) (project env) e
    case path of
      Just p | takeExtension p == ".hs", Just p /= suite -> do
        source <- splitSource <$> readText (projectRoot env </> p)
        let blamed = case blame fns p source (errorLine e) of
              InOther ref | functionNameText (refName ref) `elem` declared p -> Unblamed
              b -> b
        pure (e, path, blamed)
      _ -> pure (e, path, Unblamed)

-- | The design step that wrote the code every error is in, the earliest if
-- several: step 1 a plan module's types and imports, step 2 the signatures
-- of designed and repaired functions, step 3 the property suite. 'Nothing'
-- if any error is elsewhere.
owningStep :: [Component] -> Artefact -> [(GhcError, Maybe FilePath, Blame)] -> Maybe ToolName
owningStep cs art blamed
  | null blamed = Nothing
  | otherwise = traverse owner blamed >>= earliest
  where
    suite = suiteFile <$> suiteOf cs art
    planPaths = map planPath (modules art)
    owner (_, Just p, _) | Just p == suite = Just SetProperties
    owner (_, _, InPlanned _) = Just SetFunctions
    owner (_, Just p, Unblamed) | p `elem` planPaths = Just SetTypes
    owner _ = Nothing
    earliest owners = case [t | t <- [SetTypes, SetFunctions, SetProperties], t `elem` owners] of
      t : _ -> Just t
      [] -> Nothing

-- | Changed functions that keep their old equations until implemented.
keptBodies :: Env -> IO [FunctionRef]
keptBodies env = (\run -> [fnRef f | f <- functions (artefact run), fnPlaceholder f == PreviousBody, fnBody f == Nothing]) <$> readRun (progress env)

withUndefined :: [FunctionRef] -> Run -> Run
withUndefined refs run =
  run {artefact = (artefact run) {functions = [if fnRef f `elem` refs then f {fnPlaceholder = Undefined} else f | f <- functions (artefact run)]}}

repairRefs :: Env -> IO [FunctionRef]
repairRefs env = (\run -> [fnRef f | f <- functions (artefact run), fnKind f == Repair]) <$> readRun (progress env)

-- | Each function's declaration before the run.
previousDecls :: Env -> [FunctionRef] -> IO [(FunctionRef, Text)]
previousDecls env refs = forM refs $ \ref -> do
  before <- original (projectRoot env) (originals env) (refPath ref)
  pure (ref, maybe "" declText (before >>= findDecl (functionNameText (refName ref)) . splitSource))

declText :: Decl -> Text
declText = T.stripEnd . T.unlines . declLines

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
-- sees the design, its module with the run's functions as holes, its
-- signature and documentation or previous definition, the properties that
-- mention it, and any feedback from a failed attempt. Functions not listed
-- keep their current definitions. Returns the functions whose agents did
-- not submit an implementation.
implementStep :: Env -> [(FunctionRef, Maybe Text)] -> ExceptT () IO [FunctionRef]
implementStep env work = do
  run <- liftIO (readRun (progress env))
  let art = artefact run
      holes = art {functions = [f {fnBody = Nothing, fnImports = []} | f <- functions art]}
      queued = map fst work
  sources <- liftIO . forM (nub (map (refPath . fst) work)) $ \path -> do
    before <- original (projectRoot env) (originals env) path
    pure (path, either id id (renderFile holes path before))
  liftIO $
    update
      (progress env)
      ( \r ->
          (setStep Implement Running r)
            { tasks = [(fnRef f, if fnRef f `elem` queued then Queued else Done) | f <- functions art]
            }
      )
  slots <- liftIO (newQSem 4)
  liftIO $
    forConcurrently_ work $ \(ref, feedback) ->
      bracket_ (waitQSem slots) (signalQSem slots) $
        case [f | f <- functions art, fnRef f == ref] of
          fn : _ ->
            implementOne
              env
              (designSummary art)
              (fromMaybe "" (lookup (refPath ref) sources))
              fn
              [p | p <- properties art, mentions (fnName fn) (propSource p)]
              feedback
          [] -> update (progress env) (setTask ref (TaskFailed "Unknown function"))
  after <- liftIO (readRun (progress env))
  let missing = [ref | (ref, TaskFailed _) <- tasks after]
  liftIO $ update (progress env) $ setStep Implement $
    if null missing
      then Passed
      else Failed ("No implementation for " <> T.intercalate ", " (map (functionNameText . refName) missing) <> ".")
  pure missing

implementOne :: Env -> Text -> Text -> FunctionSpec -> [PropertySpec] -> Maybe Text -> IO ()
implementOne env design moduleSource fn props feedback = do
  update (progress env) (setTask (fnRef fn) Working)
  let chat =
        Chat
          [ userMessage (implementInstruction design moduleSource fn props feedback),
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
              Right args -> tryUpdate (progress env) (applyImplementation (fnRef fn) args)
            case result of
              Left err -> pure ("Rejected: " <> err)
              Right () -> onAccepted >> pure "Accepted."
  outcome <- try (untilAccepted env 6 ImplementFunction handler chat)
  update (progress env) $ setTask (fnRef fn) $ case outcome of
    Right (True, _) -> Done
    Right (False, _) -> TaskFailed "The model did not submit an implementation."
    Left err -> TaskFailed (T.pack (displayException (err :: SomeException)))

-- | Runs the property suite. A failed run returns its output.
propertiesStep :: Env -> ExceptT () IO (Either Text ())
propertiesStep env = do
  liftIO (update (progress env) (setStep RunProperties Running))
  run <- liftIO (readRun (progress env))
  suite <- maybe (stepFailed env RunProperties "No module was chosen.") pure (suiteOf (components (project env)) (artefact run))
  result <- liftIO (runSuite (projectRoot env) (suiteCabalTarget suite))
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
systemMessage text = Message System (Just text) [] Nothing Nothing Nothing

userMessage :: Text -> Message
userMessage text = Message User (Just text) [] Nothing Nothing Nothing
