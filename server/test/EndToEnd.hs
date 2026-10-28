{-# LANGUAGE OverloadedStrings #-}

-- | Runs requirements through 'runHoley' with a scripted model against a
-- throwaway cabal project, using the real cabal and GHC.
module EndToEnd (endToEndTests) where

import Core
import Data.Aeson (Value, object, (.=))
import Data.IORef
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import Holey (Config (..), runHoley)
import Progress (newProgress, readRun)
import ScriptedModel
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Tools (registerTool, toolToApiDefinition)
import Verify (buildSuite)

-- | Returns failure messages; an empty list means every check passed.
endToEndTests :: IO [String]
endToEndTests =
  concat
    <$> sequence
      [ scenario "first attempt" libraryProject (const correctMerge) (inspect firstAttemptPasses),
        scenario "failed property" libraryProject (onFeedback wrongMerge correctMerge) (inspect reimplementsAfterProperty),
        scenario "type error" libraryProject (onFeedback illTypedMerge correctMerge) (inspect reimplementsAfterTypeError),
        scenario "always wrong" libraryProject (const wrongMerge) (inspect givesUp),
        scenario "executable" executableProject (const correctMerge) executableStillBuilds
      ]

-- | A throwaway cabal package with one component for the run to target.
data Layout = Layout
  { stanza :: [String],
    seedFile :: (FilePath, String),
    component :: Text,
    sourceDir :: FilePath,
    moduleField :: String
  }

libraryProject, executableProject :: Layout
libraryProject =
  Layout
    { stanza = ["library", "    hs-source-dirs: src", "    exposed-modules: Demo", "    build-depends: base", "    default-language: Haskell2010"],
      seedFile = ("src/Demo.hs", "module Demo where\n\ngreeting :: String\ngreeting = \"hi\"\n"),
      component = "demo",
      sourceDir = "src",
      moduleField = "exposed-modules"
    }
executableProject =
  Layout
    { stanza = ["executable demo", "    main-is: Main.hs", "    hs-source-dirs: app", "    build-depends: base", "    default-extensions: ScopedTypeVariables", "    default-language: Haskell2010"],
      seedFile = ("app/Main.hs", "module Main (main) where\n\nmain :: IO ()\nmain = putStrLn \"hi\"\n"),
      component = "demo:exe:demo",
      sourceDir = "app",
      moduleField = "other-modules"
    }

-- | How the scripted implementer defines @merge@, given its instructions.
type MergeStrategy = Text -> Text

correctMerge, wrongMerge, illTypedMerge :: Text
correctMerge =
  T.unlines
    [ "merge (Sorted xs) (Sorted ys) = Sorted (go xs ys)",
      "  where",
      "    go [] bs = bs",
      "    go as' [] = as'",
      "    go (a : as') (b : bs)",
      "      | a <= b = a : go as' (b : bs)",
      "      | otherwise = b : go (a : as') bs"
    ]
wrongMerge = "merge (Sorted xs) (Sorted ys) = Sorted (xs ++ ys)"
illTypedMerge = "merge (Sorted xs) (Sorted ys) = xs ++ ys"

-- | The first definition until the instructions include feedback, then the second.
onFeedback :: Text -> Text -> MergeStrategy
onFeedback before after given
  | "A previous implementation failed" `T.isInfixOf` given = after
  | otherwise = before

scenario :: String -> Layout -> MergeStrategy -> (FilePath -> Run -> String -> IO [String]) -> IO [String]
scenario label layout strategy checks = do
  failures <- withProject layout $ \root -> do
    counter <- newIORef 0
    definitions <- mapM (\(name, _) -> (,) name <$> toolToApiDefinition name) registerTool
    events <- newIORef []
    progress <- newProgress (\e -> modifyIORef events (e :)) (newRun (Requirement "Merge sorted lists of integers"))
    runHoley
      Config
        { llm = scriptedLlm counter (pure . designer (component layout) strategy),
          llmModel = Mercury3,
          toolDefinitions = definitions
        }
      root
      progress
    run <- readRun progress
    cabal <- readFile (root </> "demo.cabal")
    source <- readFile (root </> modulePath)
    published <- readIORef events
    specific <- checks root run source
    pure $
      specific
        ++ concat
          [ expect ("touched files: " <> show (touchedFiles run)) $
              touchedFiles run == [modulePath, "test/HoleySortedMergeProperties.hs", "demo.cabal"],
            expect "module listed in the component once" (length (filter (== ("    " <> moduleField layout <> ": Sorted.Merge")) (lines cabal)) == 1),
            expect "property suite added once" (length (filter (== "test-suite holey-sorted-merge") (lines cabal)) == 1),
            expect "every change published" (case published of RunUpdated latest : _ -> latest == run; _ -> False)
          ]
  pure (map ((label <> ": ") <>) failures)
  where
    modulePath = sourceDir layout </> "Sorted/Merge.hs"

allPassed :: Run -> [String]
allPassed run =
  concat
    [ expect ("every step passed: " <> show (steps run)) $ all ((== Passed) . snd) (steps run),
      expect "every function implemented" $ map snd (tasks run) == [Done, Done, Done]
    ]

firstAttemptPasses :: Run -> String -> [String]
firstAttemptPasses run source =
  allPassed run
    ++ concat
      [ expect "one iteration" (iteration run == 1 && null (history run)),
        expect "no holes left" (not ("undefined" `isInfixOf` source)),
        expect "implementation imports added" ("import Data.List (sort)" `isInfixOf` source)
      ]

reimplementsAfterProperty :: Run -> String -> [String]
reimplementsAfterProperty run source =
  allPassed run
    ++ concat
      [ expect ("two iterations: " <> show (history run)) (iteration run == 2),
        expect ("reimplemented the suspects: " <> show (history run)) $
          history run == ["Iteration 1: properties failed: prop_mergeIsSortedUnion, so reimplementing toList, merge."],
        expect "fixed merge kept" ("a <= b" `isInfixOf` source)
      ]

reimplementsAfterTypeError :: Run -> String -> [String]
reimplementsAfterTypeError run _ =
  allPassed run
    ++ expect
      ("reimplemented only merge: " <> show (history run))
      (history run == ["Iteration 1: type errors in merge, so reimplementing merge."])

givesUp :: Run -> String -> [String]
givesUp run _ =
  concat
    [ expect ("stopped at properties: " <> show (steps run)) $
        case lookup RunProperties (steps run) of
          Just (Failed _) -> True
          _ -> False,
      expect ("four iterations: " <> show (iteration run)) (iteration run == 4),
      expect ("reimplemented, redesigned, reimplemented, gave up: " <> show (history run)) $
        case history run of
          [first, second, third, final] ->
            "reimplementing" `T.isInfixOf` first
              && "redesigning" `T.isInfixOf` second
              && "reimplementing" `T.isInfixOf` third
              && "Gave up after 4 iterations" `T.isPrefixOf` final
          _ -> False
    ]

-- | Checks of the finished run and its module that need nothing else.
inspect :: (Run -> String -> [String]) -> FilePath -> Run -> String -> IO [String]
inspect checks _ run source = pure (checks run source)

-- | Passes, and the executable still builds with the module registered.
executableStillBuilds :: FilePath -> Run -> String -> IO [String]
executableStillBuilds root run source = do
  built <- buildSuite root "demo:exe:demo"
  pure (firstAttemptPasses run source ++ expect ("executable builds: " <> either T.unpack (const "") built) (built == Right ()))

-- | Replies for a sorted-merge module in the given component: its design,
-- and implementations with @merge@ defined by the strategy.
designer :: Text -> MergeStrategy -> Chat -> Reply
designer target strategy chat = case latestText chat of
  Just (Tool, _) -> Say "Done."
  _
    | SetTypes `elem` tools' ->
        CallTool SetTypes $
          object
            [ "component" .= target,
              "module" .= ("Sorted.Merge" :: Text),
              "extensions" .= ([] :: [Text]),
              "imports" .= ([] :: [Text]),
              "types" .= ("-- | A list in ascending order.\nnewtype Sorted = Sorted [Int]\n  deriving (Eq, Show)" :: Text)
            ]
    | SetFunctions `elem` tools' ->
        CallTool SetFunctions $
          object
            [ "functions"
                .= [ fn "fromList" "[Int] -> Sorted" "Sorts the list.",
                     fn "toList" "Sorted -> [Int]" "The elements in ascending order.",
                     fn "merge" "Sorted -> Sorted -> Sorted" "Merges two sorted lists, keeping duplicates."
                   ]
            ]
    | SetProperties `elem` tools' ->
        CallTool SetProperties $
          object
            [ "imports" .= (["import Data.List (sort)"] :: [Text]),
              "helpers" .= ("instance Arbitrary Sorted where\n  arbitrary = fromList <$> arbitrary" :: Text),
              "properties"
                .= [ object
                       [ "name" .= ("prop_mergeIsSortedUnion" :: Text),
                         "source" .= ("prop_mergeIsSortedUnion :: Sorted -> Sorted -> Bool\nprop_mergeIsSortedUnion a b = toList (merge a b) == sort (toList a ++ toList b)" :: Text)
                       ]
                   ]
            ]
    | ImplementFunction `elem` tools' -> case implementing chat >>= (`lookup` implementations (strategy (instructions chat))) of
        Just (definition, extraImports) ->
          CallTool ImplementFunction (object ["definition" .= definition, "imports" .= extraImports])
        Nothing -> Say "I do not know that function."
    | otherwise -> Say "Nothing to do."
  where
    tools' = availableTools chat
    fn :: Text -> Text -> Text -> Value
    fn name sig doc = object ["name" .= name, "signature" .= sig, "doc" .= doc]

withProject :: Layout -> (FilePath -> IO [String]) -> IO [String]
withProject layout test = do
  tmp <- getTemporaryDirectory
  stamp <- getMonotonicTimeNSec
  let root = tmp </> ("holey-e2e-" <> show stamp)
  createDirectoryIfMissing True (root </> sourceDir layout)
  writeFile (root </> "cabal.project") "packages: .\n"
  writeFile (root </> "demo.cabal") $
    unlines (["cabal-version: 2.2", "name: demo", "version: 0.1.0.0", "build-type: Simple", ""] ++ stanza layout)
  uncurry (writeFile . (root </>)) (seedFile layout)
  failures <- test root
  keep <- lookupEnv "HOLEY_KEEP_PROJECT"
  if null failures && keep == Nothing
    then removeDirectoryRecursive root
    else putStrLn ("Kept project for inspection: " <> root)
  pure failures

expect :: String -> Bool -> [String]
expect label ok = [label | not ok]

implementations :: Text -> [(Text, (Text, [Text]))]
implementations mergeDefinition =
  [ ("fromList", ("fromList = Sorted . sort", ["import Data.List (sort)"])),
    ("toList", ("toList (Sorted xs) = xs", [])),
    ("merge", (mergeDefinition, []))
  ]

instructions :: Chat -> Text
instructions chat = T.unlines [text | m <- messages chat, role m == User, Just text <- [content m]]

-- | The function an implementation agent was asked for, from its instructions.
implementing :: Chat -> Maybe Text
implementing chat =
  case [ T.takeWhile (/= ' ') rest
       | m <- messages chat,
         role m == User,
         Just text <- [content m],
         line <- T.lines text,
         Just rest <- [T.stripPrefix "Implement " line]
       ] of
    name : _ -> Just name
    [] -> Nothing
