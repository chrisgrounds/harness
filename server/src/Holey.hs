{-# LANGUAGE OverloadedStrings #-}

module Holey (Config (..), OnFailure (..), runHoley, runLogFile) where

import Agent (Llm, runAgent)
import Control.Concurrent (newMVar, newQSem, signalQSem, waitQSem, withMVar)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (Exception, SomeException, bracket_, displayException, finally, fromException, throwIO, try)
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Core
import Data.Aeson (Value (..), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as L
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Word (Word64)
import Decls (Decl (..), DeclKind (..), SourceFile (..), findDecl, splitSource)
import GHC.Clock (getMonotonicTimeNSec)
import Edits
import Policy
import Progress (Progress, publishEvent, readRun, tryUpdate, update)
import Project
import Prompts
import Render (designSummary, renderFile, usedTypes)
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesDirectoryExist, getXdgDirectory)
import System.FilePath (isAbsolute, takeDirectory, takeExtension, (</>))
import Tools (readOnlyTool)
import Verify
import Workspace

data Config = Config
  { llm :: Llm,
    llmModel :: Model,
    toolDefinitions :: ToolName -> ToolDefinition,
    onFailure :: OnFailure,
    -- | Where the run's log goes, as an absolute path; see 'runLogFile'.
    logFile :: FilePath
  }

-- | Each chat's runs log to their own file in ~/.config/harness/logs, so
-- any chat can be debugged and not only the latest; runs without a chat
-- share one. Chat ids are unique across projects.
runLogFile :: Maybe Int -> IO FilePath
runLogFile chat = (</> maybe "last-run.log" (\c -> "chat-" <> show c <> ".log") chat) <$> getXdgDirectory XdgConfig ("harness" </> "logs")

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
    -- Implementers run concurrently, so appends are serialised.
    buildLog :: Text -> IO (),
    -- | Implemented functions from earlier designs, which a redesign reuses
    -- when their specification has not changed; see 'reuseBodies'.
    reusable :: IORef [FunctionSpec],
    -- | When the current iteration began, on the monotonic clock.
    iterationStarted :: IORef Word64
  }

type Holey = ReaderT Env IO

data Stopped = Stopped
  deriving (Show)

instance Exception Stopped

runHoley :: Config -> FilePath -> Progress -> IO ()
runHoley cfg root prog = do
  origs <- newOriginals
  began <- newIORef False
  (result, seconds) <- timed . try $ do
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
            createDirectoryIfMissing True (takeDirectory (logFile cfg))
            req <- requirement <$> readRun prog
            at <- clockTime
            TIO.writeFile (logFile cfg) ("Project: " <> T.pack root <> "\nRequirement: " <> requirementText req <> "\nStarted at " <> at <> "\n\n")
            writeIORef began True
            lock <- newMVar ()
            pool <- newIORef []
            started <- getMonotonicTimeNSec >>= newIORef
            True <$ runReaderT pipeline (Env cfg root prog info origs (withMVar lock . const . TIO.appendFile (logFile cfg)) pool started)
  finished <- case result of
    Right done -> pure done
    Left err
      | Just Stopped <- fromException err -> pure False
      | otherwise -> failCurrent prog ("Internal error: " <> T.pack (displayException err)) >> pure False
  logged <- readIORef began
  when logged $ do
    at <- clockTime
    TIO.appendFile (logFile cfg) ("-- Run " <> (if finished then "finished" else "stopped") <> " at " <> at <> " after " <> showSeconds seconds <> " --\n")
  unless finished $
    when logged $ update prog (addHistory ("Each build's output is in " <> T.pack (logFile cfg) <> "."))
  unless (finished || onFailure cfg == Keep) (undo origs)
  where
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
        Left (feedback, _) -> loopBack typesStep chat Nothing (SkeletonFailed feedback)
        Right () -> do
          -- Reused only once the skeleton checks, so a reused body that no
          -- longer fits is blamed on itself by the type check, not on the design.
          kept <- reuseEarlierBodies
          current <- iteration <$> getRun
          refs <- functionRefs
          unless (null kept) $
            note (atIteration current ("kept the implementations of " <> describeWork refs kept <> " from the previous design."))
          fns <- functions . artefact <$> getRun
          build typesStep chat Nothing [(fnRef f, Nothing) | f <- fns, isNothing (fnBody f)]
    stepOf SetTypes = DesignTypes
    stepOf SetFunctions = DesignSignatures
    stepOf _ = WriteProperties

    -- | @retrying@ is the outcome that sent the run back to reimplement this work.
    build :: Text -> Chat -> Maybe Outcome -> [(FunctionRef, Maybe Text)] -> Holey ()
    build typesStep chat retrying work = do
      before <- bodiesOf (map fst work)
      missing <- implementStep work
      after <- bodiesOf (map fst work)
      case retrying of
        _ | not (null missing) -> loopBack typesStep chat retrying (ImplementationMissing missing)
        Just outcome | not (null work), all isJust before, after == before -> do
          current <- iteration <$> getRun
          refs <- functionRefs
          let unchanged = "reimplementing " <> describeWork refs (map fst work) <> " gave back the same code"
          modifyRun (setStep Implement (Failed (T.toUpper (T.take 1 unchanged) <> T.drop 1 unchanged <> ".")))
          act typesStep chat outcome (retriedUnchanged current (map fst work) outcome) (unchanged <> ", so redesigning.")
        _ -> do
          typed <- typeCheck
          case typed of
            Left (attributed, rest) -> loopBack typesStep chat retrying (TypeErrors attributed rest)
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
                  loopBack typesStep chat retrying (PropertiesFailed failed suspects (propertyReport output))
                Right () -> do
                  (gap, reviewed) <- reviewStep chat
                  case gap of
                    Nothing -> finishIteration
                    Just reason -> loopBack typesStep reviewed retrying (RequirementUnmet reason)

    loopBack :: Text -> Chat -> Maybe Outcome -> Outcome -> Holey ()
    loopBack typesStep chat retrying outcome = do
      current <- iteration <$> getRun
      refs <- functionRefs
      let decision = decide current refs (maybe [] failedChecks retrying) outcome
      act typesStep chat outcome decision $ case decision of
        Reimplement work -> describeOutcome outcome <> ", so reimplementing " <> describeWork refs (map fst work) <> "."
        _ -> describeOutcome outcome <> ": " <> redesignReason outcome

    -- | Carries out a decision, noting @why@ in the history.
    act :: Text -> Chat -> Outcome -> Decision -> Text -> Holey ()
    act typesStep chat outcome decision why = do
      current <- iteration <$> getRun
      finishIteration
      case decision of
        GiveUp reason -> note reason >> liftIO (throwIO Stopped)
        Redesign feedback -> do
          note (atIteration current why)
          shelveBodies (implicated outcome)
          modifyRun (restartFrom Discover)
          design typesStep (addMessage (textMessage User (revisionNote (current + 1) feedback)) chat)
        Reimplement work -> do
          note (atIteration current why)
          modifyRun (restartFrom Implement)
          build typesStep chat (Just outcome) work

-- | Moves every implemented body out of the design into the pool a redesign
-- reuses from, except those of the functions the failure blamed.
shelveBodies :: [FunctionRef] -> Holey ()
shelveBodies blamed = do
  fns <- functions . artefact <$> getRun
  pool <- asks reusable
  let implemented = [f | f <- fns, isJust (fnBody f), fnRef f `notElem` blamed]
  liftIO . modifyIORef' pool $ \old ->
    implemented ++ [f | f <- old, fnRef f `notElem` map fnRef implemented, fnRef f `notElem` blamed]
  modifyRun (overFunctions (map (\f -> f {fnBody = Nothing, fnImports = []})))

-- | Restores pooled bodies whose specification is unchanged, returning the
-- functions that got one.
reuseEarlierBodies :: Holey [FunctionRef]
reuseEarlierBodies = do
  pool <- asks reusable >>= liftIO . readIORef
  before <- functions . artefact <$> getRun
  let after = reuseBodies pool before
  modifyRun (overFunctions (const after))
  pure [fnRef a | (b, a) <- zip before after, isNothing (fnBody b), isJust (fnBody a)]

bodiesOf :: [FunctionRef] -> Holey [Maybe Text]
bodiesOf refs = do
  fns <- functions . artefact <$> getRun
  pure [fnBody f | ref <- refs, f <- fns, fnRef f == ref]

-- | Logs how long the current iteration took and starts timing the next.
finishIteration :: Holey ()
finishIteration = do
  Env {iterationStarted = started, buildLog = appendLog} <- ask
  now <- liftIO getMonotonicTimeNSec
  begun <- liftIO (readIORef started)
  liftIO (writeIORef started now)
  current <- iteration <$> getRun
  liftIO . appendLog $ "-- Iteration " <> T.pack (show current) <> " took " <> showSeconds (fromIntegral (now - begun) / 1e9) <> " --\n"

maxFixUps :: Int
maxFixUps = 3

-- | Enough that a typical design's functions are all implemented at once,
-- so the step takes as long as its slowest function.
concurrentImplementers :: Int
concurrentImplementers = 16

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
      | name == tool -> Recorded <$> record
      -- A later step may correct an earlier one rather than carry its mistake
      -- through implementation to a failed check and a redesign.
      | name `elem` earlier -> Answer . either ("Rejected: " <>) (const (toolNameText name <> " is updated. Now call " <> toolNameText tool <> " to finish this step.")) <$> record
      | name == RequirementNotMet && not (null earlier) ->
          pure . Answer $
            "RequirementNotMet is for the review. During design, correct the design instead: call "
              <> T.intercalate " or " (map toolNameText earlier)
              <> " again with the correction, then "
              <> toolNameText tool
              <> "."
      | otherwise -> pure (Answer unavailable)
  where
    function = toolRequestFunction request
    name = toolRequestFunctionParamsName function
    args = toolRequestFunctionParamsArguments function
    unavailable = toolNameText name <> " is not available in this step."
    earlier = case break (== tool) [SetPlan, SetTypes, SetFunctions, SetProperties] of
      (before, _ : _) -> before
      _ -> []
    record = case name of
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

setTypes :: TypesArgs -> Holey (Either Text ())
setTypes args@(TypesArgs _ removals) = do
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
      case mergeModules plans removals (modules (artefact run)) of
        Left err -> pure (Left err)
        Right merged -> do
          clash <- case suiteOf (components info) (artefact run) {modules = merged} of
            Just suite -> liftIO (existingFiles root run [suiteFile suite])
            Nothing -> pure []
          case [p | p <- plans, planIsNew p, planModule p == ModuleName "Main"] of
            p : _ -> pure (Left ("A new module cannot be called Main: " <> componentNameText (planComponent p) <> " already has its Main. Choose another name."))
            []
              | null clash -> tryModifyRun (applyModules merged)
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
  (built, seconds) <- liftIO (timed (buildAll root))
  logCommand step "Build" built seconds
  pure (() <$ built)

-- | Logs a cabal command's verdict, duration and output under the current iteration.
logCommand :: Step -> Text -> Either Text Text -> Double -> Holey ()
logCommand step what result seconds = do
  current <- iteration <$> getRun
  appendLog <- asks buildLog
  at <- liftIO clockTime
  liftIO . appendLog $
    "== Iteration " <> T.pack (show current) <> ", " <> stepLabel step <> ": " <> either (const "failed") (const "passed") result <> " ==\n"
      <> "-- " <> what <> " took " <> showSeconds seconds <> ", finished at " <> at <> " --\n"
      <> either id id result
      <> "\n"

clockTime :: IO Text
clockTime = T.pack . formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime

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
  modelTime <- liftIO (newIORef 0)
  calls <- liftIO (newIORef (0 :: Int))
  let respond request = do
        runReaderT (modifyRun (startToolCall request)) env
        (reply, seconds) <- timed (runReaderT (handler request) env) `finally` runReaderT (modifyRun (finishToolCall request)) env
        buildLog env ("-- " <> toolNameText (toolRequestFunctionParamsName (toolRequestFunction request)) <> " took " <> showSeconds seconds <> " --\n")
        case reply of
          Answer text -> pure text
          Recorded (Left err) -> pure ("Rejected: " <> err)
          Recorded (Right ()) -> writeIORef accepted True >> pure "Accepted."
      reporting c = do
        (replied, seconds) <- timed (llm (config env) c)
        modifyIORef' modelTime (+ seconds)
        modifyIORef' calls (+ 1)
        let reply = listToMaybe (messages replied)
        mapM_ (publishEvent (progress env) . TokenRate) (reply >>= tokensPerSecond)
        mapM_ (buildLog env . logReply (model c) seconds (length (messages c))) reply
        pure replied
      attempt :: Int -> Chat -> IO Chat
      attempt remaining c = do
        c' <- runAgent reporting budget (readIORef accepted) respond c
        done <- readIORef accepted
        if done || remaining <= 1
          then pure c'
          else attempt (remaining - 1) (addMessage (textMessage User (callToolNudge tool)) c')
  liftIO $ do
    (final, total) <- timed (attempt attempts chat)
    done <- readIORef accepted
    inModel <- readIORef modelTime
    made <- readIORef calls
    buildLog env $
      "-- " <> toolNameText tool <> " finished in " <> showSeconds total <> ": " <> T.pack (show made) <> " model calls took "
        <> showSeconds inModel <> ", tools and overhead took " <> showSeconds (total - inModel) <> " --\n"
    pure (done, final)

timed :: IO a -> IO (a, Double)
timed action = do
  start <- getMonotonicTimeNSec
  result <- action
  end <- getMonotonicTimeNSec
  pure (result, fromIntegral (end - start) / 1e9)

showSeconds :: Double -> Text
showSeconds seconds = T.pack (show (fromIntegral (round (seconds * 100) :: Integer) / 100 :: Double)) <> "s"

logReply :: Model -> Double -> Int -> Message -> Text
logReply from seconds sent m =
  T.unlines $
    [ "-- Reply from " <> modelId from <> " in " <> showSeconds seconds <> " ("
        <> maybe "" (\rate -> T.pack (show (round rate :: Int)) <> " tokens/s, ") (tokensPerSecond m)
        <> T.pack (show sent) <> " messages sent) --"
    ]
      ++ maybe [] (\t -> [t | not (T.null (T.strip t))]) (content m)
      ++ ["Calls " <> toolNameText (toolRequestFunctionParamsName f) <> ": " <> toolRequestFunctionParamsArguments f | r <- toolCalls m, let f = toolRequestFunction r]
      ++ [ "No text or tool calls; output items: " <> TE.decodeUtf8 (L.toStrict (encode (readable items)))
         | null (toolCalls m), maybe True (T.null . T.strip) (content m), Just items <- [providerContent m]
         ]
  where
    readable (Array items) = Array (fmap readable items)
    readable (Object o) = Object (KM.delete "encrypted_content" o)
    readable v = v

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
  slots <- liftIO (newQSem concurrentImplementers)
  (_, seconds) <- liftIO . timed . forConcurrently_ work $ \(ref, feedback) ->
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
  unless (null work) . liftIO . buildLog env $
    "-- Implementing " <> T.pack (show (length work)) <> " functions took " <> showSeconds seconds <> " --\n"
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
    Just suite -> do
      (ran, seconds) <- liftIO (timed (runSuite root (suiteCabalTarget suite)))
      logCommand RunProperties "Properties" ran seconds
      pure (fmap (const ()) ran)

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
        appendLog <- asks buildLog
        liftIO . appendLog $
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
