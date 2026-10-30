{-# LANGUAGE OverloadedStrings #-}

module Holey (Config (..), OnFailure (..), runHoley) where

import Agent (Llm, runAgent)
import Control.Concurrent (newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (Exception, SomeException, bracket_, displayException, fromException, throwIO, try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Core
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Decls (Decl (..), DeclKind (..), SourceFile (..), findDecl, splitSource)
import Edits
import Policy
import Progress (Progress, publishEvent, readRun, tryUpdate, update)
import Project
import Prompts
import Render (designSummary, renderFile, usedTypes)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (isAbsolute, takeDirectory, takeExtension, (</>))
import Tools (readOnlyTool)
import Verify
import Workspace

data Config = Config
  { llm :: Llm,
    llmModel :: Model,
    toolDefinitions :: ToolName -> ToolDefinition,
    onFailure :: OnFailure
  }

data OnFailure
  = Keep
  | Revert
  deriving (Eq, Show)

data Env = Env
  { config :: Config,
    projectRoot :: FilePath,
    progress :: Progress,
    project :: ProjectInfo,
    originals :: Originals,
    buildLog :: FilePath
  }

type Holey = ReaderT Env IO

data Stopped = Stopped
  deriving (Show)

instance Exception Stopped

runHoley :: Config -> FilePath -> Progress -> IO ()
runHoley cfg root prog = do
  origs <- newOriginals
  result <- try $ do
    exists <- doesDirectoryExist root
    if not (isAbsolute root && exists)
      then failStart ("The project directory " <> T.pack root <> " is not an absolute path to an existing directory.")
      else do
        dirty <- uncommittedChanges root
        when dirty $
          update prog (addWarning "The working tree has uncommitted changes, so this run's edits will be mixed in with them.")
        info <- findProject root
        if null (components info)
          then failStart ("No cabal library or executable was found in " <> T.pack root <> ".")
          else do
            createDirectoryIfMissing True (takeDirectory (root </> logFile))
            req <- requirement <$> readRun prog
            TIO.writeFile (root </> logFile) ("Requirement: " <> requirementText req <> "\n\n")
            True <$ runReaderT pipeline (Env cfg root prog info origs (root </> logFile))
  finished <- case result of
    Right done -> pure done
    Left err
      | Just Stopped <- fromException err -> pure False
      | otherwise -> failCurrent prog ("Internal error: " <> T.pack (displayException err)) >> pure False
  unless finished $ do
    logged <- doesFileExist (root </> logFile)
    when logged $ update prog (addHistory ("Each build's output is in " <> T.pack logFile <> "."))
  unless (finished || onFailure cfg == Keep) (undo origs)
  where
    logFile = "dist-newstyle" </> "holey" </> "last-run.log"
    failStart reason = update prog (setStep Discover (Failed reason)) >> pure False
    undo origs = do
      run <- readRun prog
      undone <- try (rollback root origs run)
      update prog $ case undone of
        Right [] -> id
        Right paths -> addHistory ("The run did not finish, so its changes to " <> T.pack (show (length paths)) <> " files were undone.") . (\r -> r {touchedFiles = []})
        Left err -> addWarning ("Could not undo the run's changes: " <> T.pack (displayException (err :: SomeException)))

pipeline :: Holey ()
pipeline = do
  req <- requirement <$> getRun
  cs <- asks (components . project)
  m <- asks (llmModel . config)
  let typesStep = typesInstruction req cs
  design typesStep (Chat [textMessage System designSystemPrompt] m [])
  where
    design :: Text -> Chat -> Holey ()
    design typesStep chat = do
      req <- requirement <$> getRun
      cs <- asks (components . project)
      planned <- designStep Discover SetPlan (discoveryInstruction req cs) chat
      chat1 <- designStep DesignTypes SetTypes typesStep planned
      chat2 <- designStep DesignSignatures SetFunctions (functionsInstruction (model chat1)) chat1
      chat3 <- designStep WriteProperties SetProperties propertiesInstruction chat2
      verify typesStep chat3 maxFixUps

    verify :: Text -> Chat -> Int -> Holey ()
    verify typesStep chat fixUps = do
      skeleton <- checkSkeleton
      case skeleton of
        Left (feedback, Just tool) | fixUps > 0 -> do
          current <- iteration <$> getRun
          note . atIteration current $
            describeOutcome (SkeletonFailed feedback) <> ": " <> redesignReason (SkeletonFailed feedback) <> "; asking " <> toolNameText tool <> " to fix it."
          fixed <- designStep (stepOf tool) tool (fixUpInstruction tool feedback) chat
          verify typesStep fixed (fixUps - 1)
        Left (feedback, _) -> loopBack typesStep chat [] (SkeletonFailed feedback)
        Right () -> do
          refs <- functionRefs
          build typesStep chat [] [(ref, Nothing) | ref <- refs]
    stepOf SetTypes = DesignTypes
    stepOf SetFunctions = DesignSignatures
    stepOf _ = WriteProperties

    build :: Text -> Chat -> [Text] -> [(FunctionRef, Maybe Text)] -> Holey ()
    build typesStep chat previouslyFailed work = do
      missing <- implementStep work
      if not (null missing)
        then loopBack typesStep chat previouslyFailed (ImplementationMissing missing)
        else do
          typed <- typeCheck
          case typed of
            Left (attributed, rest) -> loopBack typesStep chat previouslyFailed (TypeErrors attributed rest)
            Right () -> do
              checked <- propertiesStep
              case checked of
                Left output -> do
                  art <- artefact <$> getRun
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
                Right () -> do
                  (gap, reviewed) <- reviewStep chat
                  forM_ gap (loopBack typesStep reviewed previouslyFailed . RequirementUnmet)

    loopBack :: Text -> Chat -> [Text] -> Outcome -> Holey ()
    loopBack typesStep chat previouslyFailed outcome = do
      current <- iteration <$> getRun
      refs <- functionRefs
      case decide current refs previouslyFailed outcome of
        GiveUp reason -> note reason >> liftIO (throwIO Stopped)
        Redesign feedback -> do
          note (atIteration current (describeOutcome outcome <> ": " <> redesignReason outcome))
          modifyRun (restartFrom Discover)
          design typesStep (addMessage (textMessage User (revisionNote (current + 1) feedback)) chat)
        Reimplement work -> do
          note (atIteration current (describeOutcome outcome <> ", so reimplementing " <> describeWork refs (map fst work) <> "."))
          modifyRun (restartFrom Implement)
          build typesStep chat (failedChecks outcome) work

maxFixUps :: Int
maxFixUps = 3

atIteration :: Int -> Text -> Text
atIteration n text = "Iteration " <> T.pack (show n) <> ": " <> text

getRun :: Holey Run
getRun = asks progress >>= liftIO . readRun

modifyRun :: (Run -> Run) -> Holey ()
modifyRun f = asks progress >>= \p -> liftIO (update p f)

tryModifyRun :: (Run -> Either Text Run) -> Holey (Either Text ())
tryModifyRun f = asks progress >>= \p -> liftIO (tryUpdate p f)

note :: Text -> Holey ()
note = modifyRun . addHistory

stop :: Step -> Text -> Holey a
stop step reason = modifyRun (setStep step (Failed reason)) >> liftIO (throwIO Stopped)

inStep :: Step -> (e -> Text) -> Holey (Either e a) -> Holey (Either e a)
inStep step describe action = do
  modifyRun (setStep step Running)
  result <- action
  modifyRun (setStep step (either (Failed . describe) (const Passed) result))
  pure result

functionRefs :: Holey [FunctionRef]
functionRefs = map fnRef . functions . artefact <$> getRun

definitionsFor :: [ToolName] -> Holey [ToolDefinition]
definitionsFor names = asks (\env -> map (toolDefinitions (config env)) names)

designStep :: Step -> ToolName -> Text -> Chat -> Holey Chat
designStep step tool instruction chat = do
  modifyRun (setStep step Running)
  definitions <- definitionsFor designTools
  (done, final) <- untilAccepted 30 3 tool (designTool tool) (addMessage (textMessage User instruction) chat {tools = definitions})
  if done
    then modifyRun (setStep step Passed) >> pure final
    else stop step ("The model did not call " <> toolNameText tool <> ".")

designTools :: [ToolName]
designTools = [ReadFile, ListFiles, RipGrep, SetPlan, SetTypes, SetFunctions, SetProperties, Refactor, RequirementNotMet]

data ToolReply = Answer Text | Recorded (Either Text ())

designTool :: ToolName -> ToolRequest -> Holey ToolReply
designTool tool request = do
  root <- asks projectRoot
  case readOnlyTool root name args of
    Just run -> Answer <$> liftIO run
    Nothing
      | name /= tool -> pure (Answer unavailable)
      | otherwise ->
          Recorded <$> case name of
            SetPlan -> pure $ case decodeArgs args of
              Left err -> Left err
              Right (PlanArgs behaviour existing changes assumptions)
                | any (T.null . T.strip) [behaviour, existing, changes, assumptions] -> Left "Every plan field must be non-empty."
                | otherwise -> Right ()
            SetTypes -> either (pure . Left) setTypes (decodeArgs args)
            SetFunctions -> either (pure . Left) setFunctions (decodeArgs args)
            SetProperties -> either (pure . Left) (tryModifyRun . applyProperties) (decodeArgs args)
            Refactor -> either (pure . Left) (tryModifyRun . applyRefactor) (decodeArgs args)
            _ -> pure (Left unavailable)
  where
    function = toolRequestFunction request
    name = toolRequestFunctionParamsName function
    args = toolRequestFunctionParamsArguments function
    unavailable = toolNameText name <> " is not available in this step."

setTypes :: TypesArgs -> Holey (Either Text ())
setTypes args = do
  Env {projectRoot = root, project = info, originals = origs} <- ask
  case checkModules (components info) args of
    Left err -> pure (Left err)
    Right checked -> do
      plans <- liftIO . forM checked $ \(c, name, arg) -> do
        located <- locateModule root c name
        before <- maybe (pure Nothing) (original root origs) located
        pure
          ModulePlan
            { planComponent = componentName c,
              planModule = name,
              planPath = fromMaybe (modulePath c name) located,
              planIsNew = isNothing before,
              planExtensions = argExtensions arg,
              planImports = argImports arg,
              planTypes = argTypes arg
            }
      run <- getRun
      clash <- case suiteOf (components info) (artefact run) {modules = plans} of
        Just suite -> liftIO (existingFiles root run [suiteFile suite])
        Nothing -> pure []
      case [p | p <- plans, planIsNew p, planModule p == ModuleName "Main"] of
        p : _ -> pure (Left ("A new module cannot be called Main: " <> componentNameText (planComponent p) <> " already has its Main. Choose another name."))
        []
          | null clash -> tryModifyRun (applyModules plans)
          | otherwise -> pure (Left ("Already exists: " <> T.intercalate ", " (map T.pack clash) <> ". List another module first."))

setFunctions :: FunctionsArgs -> Holey (Either Text ())
setFunctions args = do
  plans <- modules . artefact <$> getRun
  Env {projectRoot = root, originals = origs} <- ask
  offered <- asks (implementers . llmModel . config)
  previous <- liftIO . fmap concat . forM [p | p <- plans, not (planIsNew p)] $ \p -> do
    before <- original root origs (planPath p)
    pure
      [ (FunctionRef (planPath p) (FunctionName key), declText d)
      | Just text <- [before],
        d <- sourceDecls (splitSource text),
        declKind d == ValueDecl,
        Just key <- [declKey d]
      ]
  tryModifyRun (applyFunctions offered (`Map.lookup` Map.fromList previous) args)

checkSkeleton :: Holey (Either (Text, Maybe ToolName) ())
checkSkeleton = inStep CheckSkeleton (brief . fst) (attempt (1 :: Int))
  where
    attempt rounds = do
      built <- writeAndBuild CheckSkeleton
      case built of
        Right () -> pure (Right ())
        Left output -> do
          blamed <- blameErrors output
          run <- getRun
          let fns = functions (artefact run)
              repairs = [fnRef f | f <- fns, fnKind f == Repair]
              kept = [fnRef f | f <- fns, fnPlaceholder f == PreviousBody, fnBody f == Nothing]
              others = nub [ref | (_, _, InOther ref) <- blamed]
              planned = nub [ref | (_, _, InPlanned ref) <- blamed]
              unfit = filter (`elem` kept) planned
          if (not (null others) || not (null unfit)) && rounds < 8
            then do
              previousDecls others >>= modifyRun . addRepairs
              modifyRun (withUndefined unfit)
              attempt (rounds + 1)
            else do
              cs <- asks (components . project)
              let stuck = filter (`elem` repairs) planned
                  feedback = (if null stuck then "" else brokenSignaturesNote stuck <> "\n") <> summarise output
              pure (Left (feedback, owningStep cs (artefact run) blamed))

typeCheck :: Holey (Either ([(FunctionRef, Text)], [Text]) ())
typeCheck = inStep TypeCheck (\(attributed, rest) -> brief (T.unlines (map snd attributed ++ rest))) $ do
  built <- writeAndBuild TypeCheck
  case built of
    Right () -> pure (Right ())
    Left output -> do
      blamed <- blameErrors output
      previousDecls (nub [ref | (_, _, InOther ref) <- blamed]) >>= modifyRun . addRepairs
      let owner (InPlanned ref) = Just ref
          owner (InOther ref) = Just ref
          owner Unblamed = Nothing
          attributed = [(ref, T.unlines [errorText e | (e, _, b) <- blamed, owner b == Just ref]) | ref <- nub (mapMaybe (\(_, _, b) -> owner b) blamed)]
          unblamed = [errorText e | (e, _, Unblamed) <- blamed]
          rest = if null attributed && null unblamed then [summarise output] else unblamed
      pure (Left (attributed, rest))

writeAndBuild :: Step -> Holey (Either Text ())
writeAndBuild step = writeFiles >>= either (stop step) (const (buildProject step))

writeFiles :: Holey (Either Text ())
writeFiles = do
  run <- getRun
  Env {projectRoot = root, project = info, originals = origs} <- ask
  written <- liftIO (writeArtefact root (components info) origs run)
  traverse (\touched -> modifyRun (\r -> r {touchedFiles = touched})) written

buildProject :: Step -> Holey (Either Text ())
buildProject step = do
  root <- asks projectRoot
  built <- liftIO (buildAll root)
  current <- iteration <$> getRun
  logPath <- asks buildLog
  liftIO . TIO.appendFile logPath $
    "== Iteration " <> T.pack (show current) <> ", " <> stepLabel step <> ": " <> either (const "failed") (const "passed") built <> " ==\n"
      <> either id id built
      <> "\n"
  pure (() <$ built)

blameErrors :: Text -> Holey [(GhcError, Maybe FilePath, Blame)]
blameErrors output = do
  run <- getRun
  Env {projectRoot = root, project = info} <- ask
  let art = artefact run
      fns = functions art
      suite = suiteFile <$> suiteOf (components info) art
      declared p = [key | plan <- modules art, planPath plan == p, d <- sourceDecls (splitSource (planTypes plan)), Just key <- [declKey d]]
  liftIO . forM (ghcErrors output) $ \e -> do
    path <- resolveErrorFile root info e
    case path of
      Just p | takeExtension p == ".hs", Just p /= suite -> do
        source <- splitSource <$> readText (root </> p)
        let blamed = case blame fns p source (errorLine e) of
              InOther ref | functionNameText (refName ref) `elem` declared p -> Unblamed
              b -> b
        pure (e, path, blamed)
      _ -> pure (e, path, Unblamed)

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

withUndefined :: [FunctionRef] -> Run -> Run
withUndefined refs = overFunctions (map (\f -> if fnRef f `elem` refs then f {fnPlaceholder = Undefined} else f))

previousDecls :: [FunctionRef] -> Holey [(FunctionRef, Text)]
previousDecls refs = do
  Env {projectRoot = root, originals = origs} <- ask
  liftIO . forM refs $ \ref -> do
    before <- original root origs (refPath ref)
    pure (ref, maybe "" declText (before >>= findDecl (functionNameText (refName ref)) . splitSource))

declText :: Decl -> Text
declText = T.stripEnd . T.unlines . declLines

untilAccepted :: Int -> Int -> ToolName -> (ToolRequest -> Holey ToolReply) -> Chat -> Holey (Bool, Chat)
untilAccepted budget attempts tool handler chat = do
  env <- ask
  accepted <- liftIO (newIORef False)
  let respond request = do
        runReaderT (modifyRun (setToolCall request)) env
        reply <- runReaderT (handler request) env
        case reply of
          Answer text -> pure text
          Recorded (Left err) -> pure ("Rejected: " <> err)
          Recorded (Right ()) -> writeIORef accepted True >> pure "Accepted."
      reporting c = do
        replied <- llm (config env) c
        let reply = listToMaybe (messages replied)
        mapM_ (publishEvent (progress env) . TokenRate) (reply >>= tokensPerSecond)
        mapM_ (TIO.appendFile (buildLog env) . logReply (model c)) reply
        pure replied
      attempt :: Int -> Chat -> IO Chat
      attempt remaining c = do
        c' <- runAgent reporting budget respond c
        done <- readIORef accepted
        if done || remaining <= 1
          then pure c'
          else attempt (remaining - 1) (addMessage (textMessage User (callToolNudge tool)) c')
  liftIO $ do
    final <- attempt attempts chat
    done <- readIORef accepted
    pure (done, final)

logReply :: Model -> Message -> Text
logReply from m =
  T.unlines $
    ["-- Reply from " <> modelId from <> " --"]
      ++ maybe [] (\t -> [t | not (T.null (T.strip t))]) (content m)
      ++ ["Calls " <> toolNameText (toolRequestFunctionParamsName f) <> ": " <> toolRequestFunctionParamsArguments f | r <- toolCalls m, let f = toolRequestFunction r]

implementStep :: [(FunctionRef, Maybe Text)] -> Holey [FunctionRef]
implementStep work = do
  env@Env {projectRoot = root, originals = origs} <- ask
  art <- artefact <$> getRun
  let holes = art {functions = [f {fnBody = Nothing, fnImports = []} | f <- functions art]}
      queued = map fst work
  sources <- liftIO . forM (nub (map (refPath . fst) work)) $ \path -> do
    before <- original root origs path
    pure (path, either id id (renderFile holes path before))
  project' <- liftIO (sourceFiles root (components (project env)) >>= mapM (\path -> original root origs path))
  let existing = [s | Just s <- project']
  modifyRun (\r -> (setStep Implement Running r) {tasks = [(fnRef f, if fnRef f `elem` queued then Queued else Done) | f <- functions art]})
  slots <- liftIO (newQSem 4)
  liftIO . forConcurrently_ work $ \(ref, feedback) ->
    bracket_ (waitQSem slots) (signalQSem slots) . flip runReaderT env $
      case [f | f <- functions art, fnRef f == ref] of
        fn : _ ->
          implementOne
            (implementer (llmModel (config env)) fn feedback)
            (designSummary art)
            (usedTypes existing art fn)
            (fromMaybe "" (lookup (refPath ref) sources))
            fn
            [p | p <- properties art, mentions (fnName fn) (propSource p)]
            feedback
        [] -> modifyRun (setTask ref (TaskFailed "Unknown function"))
  missing <- (\run -> [ref | (ref, TaskFailed _) <- tasks run]) <$> getRun
  modifyRun . setStep Implement $
    if null missing
      then Passed
      else Failed ("No implementation for " <> T.intercalate ", " (map (functionNameText . refName) missing) <> ".")
  pure missing

implementer :: Model -> FunctionSpec -> Maybe Text -> Model
implementer run fn feedback = maybe id (const (largerImplementer run)) feedback (fromMaybe (defaultImplementer run) (fnModel fn))

implementOne :: Model -> Text -> Text -> Text -> FunctionSpec -> [PropertySpec] -> Maybe Text -> Holey ()
implementOne m design types moduleSource fn props feedback = do
  modifyRun (setTask (fnRef fn) Working)
  definitions <- definitionsFor [ImplementFunction]
  let chat = Chat [textMessage User (implementInstruction design types moduleSource fn props feedback), textMessage System implementSystemPrompt] m definitions
  env <- ask
  outcome <- liftIO (try (runReaderT (untilAccepted 6 3 ImplementFunction handler chat) env))
  modifyRun . setTask (fnRef fn) $ case outcome of
    Right (True, _) -> Done
    Right (False, _) -> TaskFailed "The model did not submit an implementation."
    Left err -> TaskFailed (T.pack (displayException (err :: SomeException)))
  where
    handler request
      | toolRequestFunctionParamsName (toolRequestFunction request) /= ImplementFunction = pure (Answer "Only ImplementFunction is available.")
      | otherwise = Recorded <$> either (pure . Left) (tryModifyRun . applyImplementation (fnRef fn)) (decodeArgs (toolRequestFunctionParamsArguments (toolRequestFunction request)))

propertiesStep :: Holey (Either Text ())
propertiesStep = inStep RunProperties (brief . propertyReport) runProperties

runProperties :: Holey (Either Text ())
runProperties = do
  run <- getRun
  Env {projectRoot = root, project = info} <- ask
  case suiteOf (components info) (artefact run) of
    Nothing -> pure (Left "No module was chosen.")
    Just suite -> fmap (const ()) <$> liftIO (runSuite root (suiteCabalTarget suite))

reviewStep :: Chat -> Holey (Maybe Text, Chat)
reviewStep chat = do
  modifyRun (setStep Review Running)
  run0 <- getRun
  cs <- asks (components . project)
  let before = artefact run0
      written = [(planModule p, [f | f <- functions before, refPath (fnRef f) == planPath p]) | p <- modules before]
      repaired = [f | f <- functions before, refPath (fnRef f) `notElem` map planPath (modules before)]
      untouched = [componentName c | c <- cs, componentName c `notElem` map planComponent (modules before)]
      instruction = reviewInstruction (requirement run0) (modules before) untouched (written ++ [(ModuleName (T.pack (refPath (fnRef f))), [f]) | f <- repaired])
  definitions <- definitionsFor designTools
  gap <- liftIO (newIORef Nothing)
  let handler request = case toolRequestFunctionParamsName (toolRequestFunction request) of
        RequirementNotMet ->
          Recorded <$> case decodeArgs (toolRequestFunctionParamsArguments (toolRequestFunction request)) of
            Left err -> pure (Left err)
            Right (GapArgs reason) -> liftIO (writeIORef gap (Just (T.strip reason))) >> pure (Right ())
        _ -> designTool Refactor request
  (recorded, reviewed) <- untilAccepted 30 1 Refactor handler (addMessage (textMessage User instruction) chat {tools = definitions})
  found <- liftIO (readIORef gap)
  let verdict headline details = do
        current <- iteration <$> getRun
        logPath <- asks buildLog
        liftIO . TIO.appendFile logPath $
          "== Iteration " <> T.pack (show current) <> ", " <> stepLabel Review <> ": " <> headline <> " ==\n" <> T.unlines details <> "\n"
  case found of
    Just reason -> do
      verdict "requirement not met" [reason]
      modifyRun (setStep Review (Failed reason))
      pure (Just reason, reviewed)
    Nothing -> do
      if recorded
        then do
          after <- artefact <$> getRun
          let changed = describeWork (map fnRef (functions after)) [fnRef f | f <- functions after, f `notElem` functions before]
          checked <- writeFiles >>= either (pure . Left) (const (buildProject Review)) >>= either (pure . Left) (const runProperties)
          case checked of
            Right () -> do
              verdict "requirement met, refactor kept" ["Refactored " <> changed <> "."]
              note ("Review: refactored " <> changed <> ".")
            Left output -> do
              modifyRun (overArtefact (const before))
              _ <- writeFiles
              let why = maybe "the properties failed" (<> ".") (firstProblem output)
              verdict "requirement met, refactor undone" ["Undid the refactor of " <> changed <> ", because " <> why]
              note ("Review: undid the refactor of " <> changed <> ", because " <> why)
        else case [t | m <- takeWhile ((/= Just instruction) . content) (messages reviewed), role m == Assistant, Just t <- [content m], not (T.null (T.strip t))] of
          conclusion : _ -> do
            verdict "requirement met" [T.strip conclusion]
            note ("Review: " <> T.strip (T.takeWhile (/= '\n') conclusion))
          [] -> verdict "requirement met" []
      modifyRun (setStep Review Passed)
      pure (Nothing, reviewed)

failCurrent :: Progress -> Text -> IO ()
failCurrent prog reason = do
  run <- readRun prog
  let step = case [s | (s, Running) <- steps run] of
        s : _ -> s
        [] -> Discover
  update prog (setStep step (Failed reason))
