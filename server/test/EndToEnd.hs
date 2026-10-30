{-# LANGUAGE OverloadedStrings #-}

module EndToEnd (endToEndTests) where

import Core
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.IORef
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTimeNSec)
import Holey (Config (..), OnFailure (..), runHoley)
import Policy (maxIterations)
import Progress (newProgress, readRun)
import ScriptedModel
import Control.Monad (filterM)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import Tools (loadToolDefinitions)
import Verify (buildTarget)

endToEndTests :: IO [String]
endToEndTests =
  concat
    <$> sequence
      [ scenario "first attempt" (sortedMerge libraryProject) (const correctMerge) (inspect firstAttemptPasses),
        scenario "failed property" (sortedMerge libraryProject) (onFeedback wrongMerge correctMerge) (inspect reimplementsAfterProperty),
        scenario "type error" (sortedMerge libraryProject) (onFeedback illTypedMerge correctMerge) (inspect reimplementsAfterTypeError),
        scenario "always wrong" (undone (sortedMerge libraryProject)) (const wrongMerge) givesUpAndUndoes,
        scenario "executable" (sortedMerge executableProject) (const correctMerge) executableStillBuilds,
        scenario "existing module" changedGreeting (const correctMerge) repairsCaller,
        scenario "another package" dependentPackage (const correctMerge) repairsDependent,
        scenario "new main module" (firstDraft (sortedMerge libraryProject)) (const correctMerge) dropsOldSuite,
        scenario "fixed properties" (brokenProperties (sortedMerge libraryProject)) (const correctMerge) fixesProperties,
        scenario "refactor" (refactored mergeLists (sortedMerge libraryProject)) (const correctMerge) keepsRefactor,
        scenario "bad refactor" (refactored "merge (Sorted xs) (Sorted ys) = xs ++ ys" (sortedMerge libraryProject)) (const correctMerge) undoesRefactor,
        scenario "requirement not met" ((sortedMerge libraryProject) {unmet = Just "Nothing shows the merge."}) (const correctMerge) redesignsAfterReview
      ]

data Layout = Layout
  { projectFiles :: [(FilePath, String)],
    cabalPath :: FilePath,
    moduleFile :: FilePath,
    changedFiles :: [ChangedFile],
    registration :: Maybe String,
    suiteHeader :: String,
    typesArgs :: Value,
    functionsArgs :: Value,
    propertiesArgs :: Value,
    draft :: Maybe (Value, Value, Value),
    draftUntil :: Text,
    refactoring :: Maybe Value,
    unmet :: Maybe Text,
    implementations :: Text -> [(Text, (Text, [Text]))]
  }

data Package = Package
  { packageStanza :: [String],
    packageSeed :: (FilePath, String),
    packageComponent :: Text,
    packageSourceDir :: FilePath,
    packageField :: String
  }

libraryProject, executableProject :: Package
libraryProject =
  Package
    { packageStanza = ["library", "    hs-source-dirs: src", "    exposed-modules: Demo", "    build-depends: base", "    default-language: Haskell2010"],
      packageSeed = ("src/Demo.hs", "module Demo where\n\ngreeting :: String\ngreeting = \"hi\"\n"),
      packageComponent = "demo",
      packageSourceDir = "src",
      packageField = "exposed-modules"
    }
executableProject =
  Package
    { packageStanza = ["executable demo", "    main-is: Main.hs", "    hs-source-dirs: app", "    build-depends: base", "    default-extensions: ScopedTypeVariables", "    default-language: Haskell2010"],
      packageSeed = ("app/Main.hs", "module Main (main) where\n\nmain :: IO ()\nmain = putStrLn \"hi\"\n"),
      packageComponent = "demo:exe:demo",
      packageSourceDir = "app",
      packageField = "other-modules"
    }

sortedMerge :: Package -> Layout
sortedMerge package =
  Layout
    { projectFiles = singlePackage (packageStanza package) [packageSeed package],
      cabalPath = "demo.cabal",
      moduleFile = path,
      changedFiles = [ChangedFile path NewFile, ChangedFile "test/HoleySortedMergeProperties.hs" NewFile, ChangedFile "demo.cabal" ExistingFile],
      registration = Just ("    " <> packageField package <> ": Sorted.Merge"),
      suiteHeader = "test-suite holey-sorted-merge",
      typesArgs =
        modulesArg
          (packageComponent package)
          "Sorted.Merge"
          "-- | A list in ascending order.\nnewtype Sorted = Sorted [Int]\n  deriving (Eq, Show)",
      functionsArgs =
        object
          [ "functions"
              .= [ fn "Sorted.Merge" "fromList" "[Int] -> Sorted" "Sorts the list.",
                   fn "Sorted.Merge" "toList" "Sorted -> [Int]" "The elements in ascending order.",
                   fn "Sorted.Merge" "merge" "Sorted -> Sorted -> Sorted" "Merges two sorted lists, keeping duplicates."
                 ]
          ],
      propertiesArgs =
        propertiesArg
          ["import Data.List (sort)"]
          "instance Arbitrary Sorted where\n  arbitrary = fromList <$> arbitrary"
          "prop_mergeIsSortedUnion"
          "prop_mergeIsSortedUnion :: Sorted -> Sorted -> Bool\nprop_mergeIsSortedUnion a b = toList (merge a b) == sort (toList a ++ toList b)",
      draft = Nothing,
      draftUntil = "",
      refactoring = Nothing,
      unmet = Nothing,
      implementations = \mergeDefinition ->
        [ ("fromList", ("fromList = Sorted . sort", ["import Data.List (sort)"])),
          ("toList", ("toList (Sorted xs) = xs", [])),
          ("merge", (mergeDefinition, []))
        ]
    }
  where
    path = packageSourceDir package </> "Sorted/Merge.hs"

changedGreeting :: Layout
changedGreeting =
  Layout
    { projectFiles =
        singlePackage
          [ "library",
            "    hs-source-dirs: src",
            "    exposed-modules: Demo",
            "    build-depends: base",
            "    default-language: Haskell2010",
            "",
            "test-suite demo-tests",
            "    type: exitcode-stdio-1.0",
            "    hs-source-dirs: tests",
            "    main-is: Main.hs",
            "    build-depends: base, demo",
            "    default-language: Haskell2010"
          ]
            [ ( "src/Demo.hs",
              unlines
                [ "module Demo (greeting, shout) where",
                  "",
                  "-- | How to greet.",
                  "greeting :: String",
                  "greeting = \"hi\"",
                  "",
                  "-- | The greeting, loudly.",
                  "shout :: String",
                  "shout = greeting ++ \"!\"",
                  "",
                  "-- | Says hello when shown.",
                  "data Greeter = Greeter",
                  "",
                  "instance Show Greeter where",
                  "  show Greeter = greeting"
                ]
            ),
            ( "tests/Main.hs",
              unlines
                [ "module Main (main) where",
                  "",
                  "import Demo (shout)",
                  "import System.Exit (exitFailure, exitSuccess)",
                  "",
                  "main :: IO ()",
                  "main = if last shout == '!' then exitSuccess else exitFailure"
                ]
            )
          ],
      cabalPath = "demo.cabal",
      moduleFile = "src/Demo.hs",
      changedFiles = [ChangedFile "src/Demo.hs" ExistingFile, ChangedFile "test/HoleyDemoProperties.hs" NewFile, ChangedFile "demo.cabal" ExistingFile],
      registration = Nothing,
      suiteHeader = "test-suite holey-demo",
      typesArgs = modulesArg "demo" "Demo" "-- | Who to greet.\nnewtype Name = Name String\n  deriving (Eq, Show)",
      functionsArgs =
        object
          [ "functions"
              .= [ changing (fn "Demo" "greeting" "Name -> String" "Greets the name.") "Take the name to greet.",
                   fn "Demo" "farewell" "Name -> String" "Says goodbye to the name."
                 ]
          ],
      propertiesArgs =
        propertiesArg
          ["import Data.List (isInfixOf, isPrefixOf)"]
          ""
          "prop_greetingNamesThem"
          "prop_greetingNamesThem :: String -> Bool\nprop_greetingNamesThem n = n `isInfixOf` greeting (Name n) && n `isInfixOf` farewell (Name n)",
      draft = Nothing,
      draftUntil = "",
      refactoring = Nothing,
      unmet = Nothing,
      implementations =
        const
          [ ("greeting", ("greeting (Name n) = \"hi \" ++ n", [])),
            ("farewell", ("farewell (Name n) = \"bye \" ++ n", [])),
            ("shout", ("shout = greeting (Name \"you\") ++ \"!\"", [])),
            ("instance Show Greeter", ("instance Show Greeter where\n  show Greeter = greeting (Name \"greeter\")", []))
          ]
    }

dependentPackage :: Layout
dependentPackage =
  Layout
    { projectFiles =
        [ ("cabal.project", "packages: core app\n"),
          ( "core/core.cabal",
            unlines (packageHeader "core" ++ ["library", "    hs-source-dirs: src", "    exposed-modules: Greet", "    build-depends: base", "    default-language: Haskell2010", "", "executable core-cli", "    main-is: Main.hs", "    hs-source-dirs: src", "    build-depends: base", "    default-language: Haskell2010"])
          ),
          ("core/src/Greet.hs", unlines ["module Greet (greeting) where", "", "greeting :: String", "greeting = \"hi\""]),
          ("core/src/Main.hs", unlines ["module Main (main) where", "", "main :: IO ()", "main = putStrLn \"core\""]),
          ("app/app.cabal", unlines (packageHeader "app" ++ ["executable app", "    main-is: Main.hs", "    hs-source-dirs: src", "    build-depends: base, core", "    default-language: Haskell2010"])),
          ("app/src/Main.hs", unlines ["module Main (main) where", "", "import Greet (greeting)", "", "-- | Greets loudly.", "main :: IO ()", "main = putStrLn (greeting ++ \"!\")"])
        ],
      cabalPath = "core/core.cabal",
      moduleFile = "app/src/Main.hs",
      changedFiles = [ChangedFile "core/src/Greet.hs" ExistingFile, ChangedFile "core/test/HoleyGreetProperties.hs" NewFile, ChangedFile "core/core.cabal" ExistingFile, ChangedFile "app/src/Main.hs" ExistingFile],
      registration = Nothing,
      suiteHeader = "test-suite holey-greet",
      typesArgs = modulesArg "core" "Greet" "newtype Name = Name String",
      functionsArgs = object ["functions" .= [fn "Greet" "greeting" "Name -> Maybe String" "Greets the name."]],
      propertiesArgs =
        propertiesArg
          ["import Data.List (isInfixOf, isPrefixOf)"]
          ""
          "prop_greetingNamesThem"
          "prop_greetingNamesThem :: String -> Bool\nprop_greetingNamesThem n = maybe False (n `isInfixOf`) (greeting (Name n))",
      draft = Nothing,
      draftUntil = "",
      refactoring = Nothing,
      unmet = Nothing,
      implementations =
        const
          [ ("greeting", ("greeting (Name n) = Just (\"hi \" ++ n)", [])),
            ("main", ("main = putStrLn (maybe \"\" id (greeting (Name \"you\")) ++ \"!\")", ["import Greet (Name (..))"]))
          ]
    }

packageHeader :: String -> [String]
packageHeader name = ["cabal-version: 2.2", "name: " <> name, "version: 0.1.0.0", "build-type: Simple", ""]

singlePackage :: [String] -> [(FilePath, String)] -> [(FilePath, String)]
singlePackage stanzas seeds = ("cabal.project", "packages: .\n") : ("demo.cabal", unlines (packageHeader "demo" ++ stanzas)) : seeds

modulesArg :: Text -> Text -> Text -> Value
modulesArg component moduleName types =
  object
    [ "modules"
        .= [ object
               [ "component" .= component,
                 "module" .= moduleName,
                 "extensions" .= ([] :: [Text]),
                 "imports" .= ([] :: [Text]),
                 "types" .= types
               ]
           ]
    ]

propertiesArg :: [Text] -> Text -> Text -> Text -> Value
propertiesArg imports helpers name source =
  object
    [ "imports" .= imports,
      "helpers" .= helpers,
      "properties" .= [object ["name" .= name, "source" .= source]]
    ]

fn :: Text -> Text -> Text -> Text -> Value
fn moduleName name sig doc = object ["module" .= moduleName, "name" .= name, "signature" .= sig, "doc" .= doc]

changing :: Value -> Text -> Value
changing (Object o) change = Object (o <> KeyMap.fromList [("change", String change)])
changing v _ = v

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

onFeedback :: Text -> Text -> MergeStrategy
onFeedback before after given
  | "A previous implementation failed" `T.isInfixOf` given = after
  | otherwise = before

scenario :: String -> Layout -> MergeStrategy -> (FilePath -> Run -> String -> IO [String]) -> IO [String]
scenario label layout strategy checks = do
  failures <- withProject layout $ \root -> do
    counter <- newIORef 0
    definitions <- loadToolDefinitions Mercury3
    events <- newIORef []
    progress <- newProgress (\e -> modifyIORef events (e :)) (newRun (Requirement "Merge sorted lists of integers"))
    runHoley
      Config
        { llm = scriptedLlm counter (pure . designer layout strategy),
          llmModel = Mercury3,
          toolDefinitions = definitions,
          onFailure = Revert
        }
      root
      progress
    run <- readRun progress
    cabal <- readFile (root </> cabalPath layout)
    hasModule <- doesFileExist (root </> moduleFile layout)
    source <- if hasModule then readFile (root </> moduleFile layout) else pure ""
    published <- readIORef events
    replies <- readIORef counter
    specific <- checks root run source
    let kept
          | null (changedFiles layout) = []
          | otherwise =
              concat
                [ expect "module listed in the component once" $
                    maybe True (\line -> length (filter (== line) (lines cabal)) == 1) (registration layout),
                  expect "property suite added once" (length (filter (== suiteHeader layout) (lines cabal)) == 1)
                ]
    pure $
      specific
        ++ kept
        ++ concat
          [ expect ("touched files: " <> show (touchedFiles run)) $
              touchedFiles run == changedFiles layout,
            expect "every change published" (case [r | RunUpdated r <- published] of latest : _ -> latest == run; _ -> False),
            expect ("every reply's rate published: " <> show (length [() | TokenRate _ <- published], replies)) (length [() | TokenRate _ <- published] == replies)
          ]
  pure (map ((label <> ": ") <>) failures)

allPassed :: Run -> [String]
allPassed run =
  concat
    [ expect ("every step passed: " <> show (steps run)) $ all ((== Passed) . snd) (steps run),
      expect ("every function implemented: " <> show (tasks run)) $ not (null (tasks run)) && all ((== Done) . snd) (tasks run)
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

undone :: Layout -> Layout
undone layout = layout {changedFiles = []}

givesUpAndUndoes :: FilePath -> Run -> String -> IO [String]
givesUpAndUndoes root run _ = do
  buildLog <- readFile (root </> "dist-newstyle/holey/last-run.log")
  files <- mapM (\(path, contents) -> (,) path . (== contents) <$> readFile (root </> path)) projectFiles'
  leftovers <- filterM (doesPathExist . (root </>)) ["src/Sorted", "test"]
  pure $
    concat
      [ expect ("stopped at properties: " <> show (steps run)) $
          case lookup RunProperties (steps run) of
            Just (Failed _) -> True
            _ -> False,
        expect ("used every iteration: " <> show (iteration run)) (iteration run == maxIterations),
        expect ("alternately reimplemented and redesigned, gave up, then undid its changes: " <> show (history run)) $
          length (history run) == maxIterations + 2
            && and (zipWith T.isInfixOf (cycle ["so reimplementing", "they still fail after the functions they mention were reimplemented"]) (take (maxIterations - 1) (history run)))
            && ("Gave up after " <> T.pack (show maxIterations) <> " iterations") `T.isPrefixOf` (history run !! (maxIterations - 1))
            && drop maxIterations (history run) == ["Each build's output is in dist-newstyle/holey/last-run.log.", "The run did not finish, so its changes to 3 files were undone."],
        expect ("every build logged: " <> show (length (filter ("== Iteration " `isPrefixOf`) (lines buildLog)))) $
          length (filter ("== Iteration " `isPrefixOf`) (lines buildLog)) > maxIterations
            && "== Iteration 10, Type checking: passed ==" `elem` lines buildLog,
        expect ("files restored: " <> show [p | (p, False) <- files]) (all snd files),
        expect ("created paths removed: " <> show leftovers) (null leftovers)
      ]
  where
    projectFiles' = projectFiles (sortedMerge libraryProject)

inspect :: (Run -> String -> [String]) -> FilePath -> Run -> String -> IO [String]
inspect checks _ run source = pure (checks run source)

executableStillBuilds :: FilePath -> Run -> String -> IO [String]
executableStillBuilds root run source = do
  built <- buildTarget root "demo:exe:demo"
  pure (firstAttemptPasses run source ++ expect ("executable builds: " <> either T.unpack (const "") built) (built == Right ()))

repairsCaller :: FilePath -> Run -> String -> IO [String]
repairsCaller root run source = do
  tests <- readFile (root </> "tests/Main.hs")
  pure $
    allPassed run
      ++ concat
        [ expect ("one iteration: " <> show (history run)) (iteration run == 1 && null (history run)),
          expect ("shout and the instance repaired: " <> show (map fst (tasks run))) $
            [(fnName f, fnKind f) | f <- functions (artefact run)]
              == [(FunctionName "greeting", Designed), (FunctionName "farewell", Designed), (FunctionName "shout", Repair), (FunctionName "instance Show Greeter", Repair)],
          expect "instance repaired in place" ("-- | Says hello when shown.\ndata Greeter = Greeter\n\ninstance Show Greeter where\n  show Greeter = greeting (Name \"greeter\")" `isInfixOf` source),
          expect "greeting replaced in place, keeping its original comment" ("-- | How to greet.\ngreeting :: Name -> String\ngreeting (Name n) = \"hi \" ++ n" `isInfixOf` source),
          expect "the change note is not in the code" (not ("Take the name to greet." `isInfixOf` source)),
          expect "shout keeps its documentation and signature" ("-- | The greeting, loudly.\nshout :: String\nshout = greeting (Name \"you\") ++ \"!\"" `isInfixOf` source),
          expect ("new names exported: " <> takeWhile (/= '\n') source) ("module Demo (greeting, shout, Name (..), farewell) where" `isInfixOf` source),
          expect "existing tests untouched" ("main = if last shout == '!' then exitSuccess else exitFailure" `isInfixOf` tests)
        ]

firstDraft :: Layout -> Layout
firstDraft layout =
  layout
    { changedFiles = [ChangedFile "src/Sorted/Merge.hs" NewFile, ChangedFile "demo.cabal" ExistingFile, ChangedFile "test/HoleySortedMergeProperties.hs" NewFile],
      draftUntil = "the previous design failed verification",
      draft =
        Just
          ( object
              [ "modules"
                  .= [ object ["component" .= ("demo" :: Text), "module" .= ("Demo" :: Text), "extensions" .= ([] :: [Text]), "imports" .= ([] :: [Text]), "types" .= ("" :: Text)],
                       object ["component" .= ("demo" :: Text), "module" .= ("Sorted.Merge" :: Text), "extensions" .= ([] :: [Text]), "imports" .= ([] :: [Text]), "types" .= ("newtype Sorted = Sorted [Int]\n  deriving (Eq, Show)" :: Text)]
                     ]
              ],
            functionsArgs layout,
            propertiesArg [] "" "prop_missing" "prop_missing :: Bool\nprop_missing = missingHelper"
          )
    }

dropsOldSuite :: FilePath -> Run -> String -> IO [String]
dropsOldSuite root run source = do
  cabal <- readFile (root </> "demo.cabal")
  oldSuite <- doesFileExist (root </> "test/HoleyDemoProperties.hs")
  pure $
    allPassed run
      ++ concat
        [ expect ("asked for corrected properties three times, then redesigned, saying why: " <> show (history run)) $
            case history run of
              [a, b, c, redesign] ->
                all (fixUpNote "SetProperties") [a, b, c]
                  && "Iteration 1: the skeleton did not type check: Variable not in scope: missingHelper" `T.isPrefixOf` redesign
                  && not ("asking" `T.isInfixOf` redesign)
              _ -> False,
          expect "old suite file removed" (not oldSuite),
          expect "old suite stanza removed" (not ("test-suite holey-demo" `isInfixOf` cabal)),
          expect "no holes left" (not ("undefined" `isInfixOf` source))
        ]

brokenProperties :: Layout -> Layout
brokenProperties layout =
  layout
    { draft = Just (typesArgs layout, functionsArgs layout, propertiesArg [] "" "prop_missing" "prop_missing :: Bool\nprop_missing = missingHelper"),
      draftUntil = "because of what this step wrote"
    }

refactored :: Text -> Layout -> Layout
refactored mergeDefinition layout =
  layout
    { refactoring =
        Just $
          object
            [ "functions"
                .= [ object ["module" .= ("Sorted.Merge" :: Text), "name" .= ("merge" :: Text), "signature" .= ("" :: Text), "doc" .= ("" :: Text), "definition" .= mergeDefinition, "imports" .= ([] :: [Text])],
                     object ["module" .= ("Sorted.Merge" :: Text), "name" .= ("mergeLists" :: Text), "signature" .= ("[Int] -> [Int] -> [Int]" :: Text), "doc" .= ("Merges two ascending lists." :: Text), "definition" .= mergeListsDefinition, "imports" .= ([] :: [Text])]
                   ]
            ]
    }
  where
    mergeListsDefinition =
      T.unlines
        [ "mergeLists [] bs = bs",
          "mergeLists as' [] = as'",
          "mergeLists (a : as') (b : bs)",
          "  | a <= b = a : mergeLists as' (b : bs)",
          "  | otherwise = b : mergeLists (a : as') bs"
        ]

mergeLists :: Text
mergeLists = "merge (Sorted xs) (Sorted ys) = Sorted (mergeLists xs ys)"

keepsRefactor :: FilePath -> Run -> String -> IO [String]
keepsRefactor _ run source =
  pure $
    allPassed run
      ++ concat
        [ expect ("refactor kept: " <> show (history run)) (history run == ["Review: refactored merge, mergeLists."]),
          expect "merge uses the helper" ("merge (Sorted xs) (Sorted ys) = Sorted (mergeLists xs ys)" `isInfixOf` source),
          expect "helper added without emitting its design documentation" ("mergeLists :: [Int] -> [Int] -> [Int]" `isInfixOf` source && not ("Merges two ascending lists." `isInfixOf` source))
        ]

undoesRefactor :: FilePath -> Run -> String -> IO [String]
undoesRefactor _ run source =
  pure $
    allPassed run
      ++ concat
        [ expect ("refactor undone: " <> show (history run)) (case history run of [n] -> "Review: undid the refactor of merge, mergeLists, because " `T.isPrefixOf` n; _ -> False),
          expect "the implemented merge is back" ("a <= b" `isInfixOf` source && not ("mergeLists" `isInfixOf` source))
        ]

redesignsAfterReview :: FilePath -> Run -> String -> IO [String]
redesignsAfterReview root run _ = do
  logged <- readFile (root </> "dist-newstyle/holey/last-run.log")
  pure $
    allPassed run
      ++ concat
        [ expect ("redesigned once, saying why: " <> show (history run)) (history run == ["Iteration 1: the review found the requirement not met: Nothing shows the merge."]),
          expect "two iterations" (iteration run == 2),
          expect "the verdicts are logged" $
            all (`isInfixOf` logged) ["== Iteration 1, Reviewing against the requirement: requirement not met ==\nNothing shows the merge.", "== Iteration 2, Reviewing against the requirement: requirement met =="]
        ]

fixesProperties :: FilePath -> Run -> String -> IO [String]
fixesProperties _ run source =
  pure $
    firstAttemptPasses' run source
      ++ expect ("asked once for corrected properties: " <> show (history run)) (case history run of [n] -> fixUpNote "SetProperties" n; _ -> False)
  where
    firstAttemptPasses' r s = allPassed r ++ expect "one iteration" (iteration r == 1) ++ expect "no holes left" (not ("undefined" `isInfixOf` s))

fixUpNote :: Text -> Text -> Bool
fixUpNote tool line =
  "Iteration 1: the skeleton did not type check: Variable not in scope: missingHelper" `T.isPrefixOf` line
    && ("; asking " <> tool <> " to fix it.") `T.isSuffixOf` line

repairsDependent :: FilePath -> Run -> String -> IO [String]
repairsDependent root run source = do
  coreMain <- readFile (root </> "core/src/Main.hs")
  pure $
    concat
      [ expect ("every step passed: " <> show (steps run)) $ all ((== Passed) . snd) (steps run),
        expect ("one iteration: " <> show (history run)) (iteration run == 1 && null (history run)),
        expect ("main repaired in the app: " <> show [(fnRef f, fnKind f) | f <- functions (artefact run)]) $
          [(fnRef f, fnKind f) | f <- functions (artefact run)]
            == [(FunctionRef "core/src/Greet.hs" (FunctionName "greeting"), Designed), (FunctionRef "app/src/Main.hs" (FunctionName "main"), Repair)],
        expect "the app's main keeps its documentation and gets the import" $
          all (`isInfixOf` source) ["import Greet (Name (..))", "-- | Greets loudly.\nmain :: IO ()\nmain = putStrLn (maybe \"\" id (greeting (Name \"you\")) ++ \"!\")"],
        expect "the library's executable untouched" ("main = putStrLn \"core\"" `isInfixOf` coreMain)
      ]

designer :: Layout -> MergeStrategy -> Chat -> Reply
designer layout strategy chat = case latestText chat of
  Just (Tool, _) -> Say "Done."
  _
    | ImplementFunction `elem` tools' -> case implementing chat >>= (`lookup` implementations layout (strategy (instructions chat))) of
        Just (definition, extraImports) ->
          CallTool ImplementFunction (object ["definition" .= definition, "imports" .= extraImports])
        Nothing -> Say "I do not know that function."
    | otherwise -> case step of
        Just SetTypes -> CallTool SetTypes types
        Just SetFunctions -> CallTool SetFunctions functions'
        Just SetProperties -> CallTool SetProperties properties'
        _
          | reviewing -> case (refactoring layout, unmet layout) of
              (Just changes, _) -> CallTool Refactor changes
              (_, Just reason) | not ("does not meet the requirement" `T.isInfixOf` instructions chat) -> CallTool RequirementNotMet (object ["reason" .= reason])
              _ -> Say ""
          | otherwise -> Say "Nothing to do."
  where
    tools' = availableTools chat
    step = case [text | m <- messages chat, role m == User, Just text <- [content m]] of
      latest : _ -> case [t | t <- [SetTypes, SetFunctions, SetProperties], toolNameText t `T.isInfixOf` latest] of
        t : _ -> Just t
        [] -> Nothing
      [] -> Nothing
    revised = draftUntil layout `T.isInfixOf` instructions chat
    reviewing = maybe False ("Step 4: review" `T.isInfixOf`) (listToMaybe [text | m <- messages chat, role m == User, Just text <- [content m]])
    (types, functions', properties') = case draft layout of
      Just first | not revised -> first
      _ -> (typesArgs layout, functionsArgs layout, propertiesArgs layout)

withProject :: Layout -> (FilePath -> IO [String]) -> IO [String]
withProject layout test = do
  tmp <- getTemporaryDirectory
  stamp <- getMonotonicTimeNSec
  let root = tmp </> ("holey-e2e-" <> show stamp)
  mapM_ (\(path, contents) -> createDirectoryIfMissing True (takeDirectory (root </> path)) >> writeFile (root </> path) contents) (projectFiles layout)
  failures <- test root
  keep <- lookupEnv "HOLEY_KEEP_PROJECT"
  if null failures && keep == Nothing
    then removeDirectoryRecursive root
    else putStrLn ("Kept project for inspection: " <> root)
  pure failures

expect :: String -> Bool -> [String]
expect label ok = [label | not ok]

instructions :: Chat -> Text
instructions chat = T.unlines [text | m <- messages chat, role m == User, Just text <- [content m]]

implementing :: Chat -> Maybe Text
implementing chat =
  case [ name
       | m <- messages chat,
         role m == User,
         Just text <- [content m],
         line <- T.lines text,
         Just name <- [T.takeWhile (/= ' ') <$> T.stripPrefix "Implement " line, fst . T.breakOn " so it" <$> T.stripPrefix "Rewrite " line]
       ] of
    name : _ -> Just name
    [] -> Nothing
