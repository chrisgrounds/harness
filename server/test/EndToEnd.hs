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
endToEndTests = sortedMerge

sortedMerge :: IO [String]
sortedMerge = withProject $ \root -> do
  counter <- newIORef 0
  definitions <- mapM (\(name, _) -> (,) name <$> toolToApiDefinition name) registerTool
  events <- newIORef []
  progress <- newProgress (\e -> modifyIORef events (e :)) (newRun (Requirement "Merge sorted lists of integers"))
  runHoley
    Config
      { llm = scriptedLlm counter (pure . designer),
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
    concat
      [ expect ("every step passed: " <> show (steps run)) $ all ((== Passed) . snd) (steps run),
        expect "every function implemented" $ map snd (tasks run) == [Done, Done, Done],
        expect ("touched files: " <> show (touchedFiles run)) $
          touchedFiles run == ["src/Sorted/Merge.hs", "test/HoleySortedMergeProperties.hs", "demo.cabal"],
        expect "module listed in the library" ("exposed-modules: Sorted.Merge" `isInfixOf` cabal),
        expect "property suite added" ("test-suite holey-sorted-merge" `isInfixOf` cabal),
        expect "no holes left" (not ("undefined" `isInfixOf` source)),
        expect "implementation imports added" ("import Data.List (sort)" `isInfixOf` source),
        expect "every change published" (case published of RunUpdated latest : _ -> latest == run; _ -> False)
      ]

-- | Replies for the design steps of a sorted-merge module.
designer :: Chat -> Reply
designer chat = case latestText chat of
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
    | ImplementFunction `elem` tools' -> case implementing chat >>= (`lookup` implementations) of
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

implementations :: [(Text, (Text, [Text]))]
implementations =
  [ ("fromList", ("fromList = Sorted . sort", ["import Data.List (sort)"])),
    ("toList", ("toList (Sorted xs) = xs", [])),
    ( "merge",
      ( T.unlines
          [ "merge (Sorted xs) (Sorted ys) = Sorted (go xs ys)",
            "  where",
            "    go [] bs = bs",
            "    go as' [] = as'",
            "    go (a : as') (b : bs)",
            "      | a <= b = a : go as' (b : bs)",
            "      | otherwise = b : go (a : as') bs"
          ],
        []
      )
    )
  ]

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
