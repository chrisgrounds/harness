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

-- | Returns failure messages; an empty list means every check passed.
endToEndTests :: IO [String]
endToEndTests =
  concat
    <$> sequence
      [ scenario "first attempt" (const correctMerge) firstAttemptPasses,
        scenario "failed property" (onFeedback wrongMerge correctMerge) reimplementsAfterProperty,
        scenario "type error" (onFeedback illTypedMerge correctMerge) reimplementsAfterTypeError,
        scenario "always wrong" (const wrongMerge) givesUp
      ]

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

scenario :: String -> MergeStrategy -> (Run -> String -> [String]) -> IO [String]
scenario label strategy checks = do
  failures <- withProject $ \root -> do
    counter <- newIORef 0
    definitions <- mapM (\(name, _) -> (,) name <$> toolToApiDefinition name) registerTool
    events <- newIORef []
    progress <- newProgress (\e -> modifyIORef events (e :)) (newRun (Requirement "Merge sorted lists of integers"))
    runHoley
      Config
        { llm = scriptedLlm counter (pure . designer strategy),
          llmModel = Mercury3,
          toolDefinitions = definitions,
          projectRoot = root
        }
      progress
    run <- readRun progress
    cabal <- readFile (root </> "demo.cabal")
    source <- readFile (root </> "src/Sorted/Merge.hs")
    published <- readIORef events
    pure $
      checks run source
        ++ concat
          [ expect ("touched files: " <> show (touchedFiles run)) $
              touchedFiles run == ["src/Sorted/Merge.hs", "test/HoleySortedMergeProperties.hs", "demo.cabal"],
            expect "module listed in the library once" (length (filter (== "    exposed-modules: Sorted.Merge") (lines cabal)) == 1),
            expect "property suite added once" (length (filter (== "test-suite holey-sorted-merge") (lines cabal)) == 1),
            expect "every change published" (case published of RunUpdated latest : _ -> latest == run; _ -> False)
          ]
  pure (map ((label <> ": ") <>) failures)

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

-- | Replies for a sorted-merge module: its design, and implementations with
-- @merge@ defined by the strategy.
designer :: MergeStrategy -> Chat -> Reply
designer strategy chat = case latestText chat of
  Just (Tool, _) -> Say "Done."
  _
    | SetTypes `elem` tools' ->
        CallTool SetTypes $
          object
            [ "package" .= ("demo" :: Text),
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

withProject :: (FilePath -> IO [String]) -> IO [String]
withProject test = do
  tmp <- getTemporaryDirectory
  stamp <- getMonotonicTimeNSec
  let root = tmp </> ("holey-e2e-" <> show stamp)
  createDirectoryIfMissing True (root </> "src")
  writeFile (root </> "cabal.project") "packages: .\n"
  writeFile (root </> "demo.cabal") $
    unlines
      [ "cabal-version: 2.2",
        "name: demo",
        "version: 0.1.0.0",
        "build-type: Simple",
        "",
        "library",
        "    hs-source-dirs: src",
        "    exposed-modules: Demo",
        "    build-depends: base",
        "    default-language: Haskell2010"
      ]
  writeFile (root </> "src/Demo.hs") "module Demo where\n\ngreeting :: String\ngreeting = \"hi\"\n"
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
