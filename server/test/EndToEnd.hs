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
import Policy (maxIterations)
import Progress (newProgress, readRun)
import ScriptedModel
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import Tools (registerTool, toolToApiDefinition)
import Verify (buildTarget)

-- | Returns failure messages; an empty list means every check passed.
endToEndTests :: IO [String]
endToEndTests =
  concat
    <$> sequence
      [ scenario "first attempt" (sortedMerge libraryProject) (const correctMerge) (inspect firstAttemptPasses),
        scenario "failed property" (sortedMerge libraryProject) (onFeedback wrongMerge correctMerge) (inspect reimplementsAfterProperty),
        scenario "type error" (sortedMerge libraryProject) (onFeedback illTypedMerge correctMerge) (inspect reimplementsAfterTypeError),
        scenario "always wrong" (sortedMerge libraryProject) (const wrongMerge) (inspect givesUp),
        scenario "executable" (sortedMerge executableProject) (const correctMerge) executableStillBuilds,
        scenario "existing module" changedGreeting (const correctMerge) repairsCaller,
        scenario "another package" dependentPackage (const correctMerge) repairsDependent
      ]

-- | A throwaway cabal package, and what the scripted model designs in it.
data Layout = Layout
  { -- | Every file of the project, including its cabal files.
    projectFiles :: [(FilePath, String)],
    cabalPath :: FilePath,
    -- | The file the checks read, and the files the run should change.
    moduleFile :: FilePath,
    changedFiles :: [FilePath],
    -- | The line that registers a new module, if the run creates one.
    registration :: Maybe String,
    suiteHeader :: String,
    typesArgs :: Value,
    functionsArgs :: Value,
    propertiesArgs :: Value,
    -- | Each function's definition and imports, given how to define @merge@.
    implementations :: Text -> [(Text, (Text, [Text]))]
  }

-- | A package with one component for a new sorted-merge module.
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

-- | A new module of sorted lists with @merge@.
sortedMerge :: Package -> Layout
sortedMerge package =
  Layout
    { projectFiles = singlePackage (packageStanza package) [packageSeed package],
      cabalPath = "demo.cabal",
      moduleFile = path,
      changedFiles = [path, "test/HoleySortedMergeProperties.hs", "demo.cabal"],
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
      implementations = \mergeDefinition ->
        [ ("fromList", ("fromList = Sorted . sort", ["import Data.List (sort)"])),
          ("toList", ("toList (Sorted xs) = xs", [])),
          ("merge", (mergeDefinition, []))
        ]
    }
  where
    path = packageSourceDir package </> "Sorted/Merge.hs"

-- | An existing library whose @greeting@ takes a name from now on, which
-- breaks @shout@, with a test suite that uses @shout@.
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
                  "shout = greeting ++ \"!\""
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
      changedFiles = ["src/Demo.hs", "test/HoleyDemoProperties.hs", "demo.cabal"],
      registration = Nothing,
      suiteHeader = "test-suite holey-demo",
      typesArgs = modulesArg "demo" "Demo" "-- | Who to greet.\nnewtype Name = Name String\n  deriving (Eq, Show)",
      functionsArgs =
        object
          [ "functions"
              .= [ fn "Demo" "greeting" "Name -> String" "Greets the name.",
                   fn "Demo" "farewell" "Name -> String" "Says goodbye to the name."
                 ]
          ],
      propertiesArgs =
        propertiesArg
          ["import Data.List (isInfixOf)"]
          ""
          "prop_greetingNamesThem"
          "prop_greetingNamesThem :: String -> Bool\nprop_greetingNamesThem n = n `isInfixOf` greeting (Name n) && n `isInfixOf` farewell (Name n)",
      implementations =
        const
          [ ("greeting", ("greeting (Name n) = \"hi \" ++ n", [])),
            ("farewell", ("farewell (Name n) = \"bye \" ++ n", [])),
            ("shout", ("shout = greeting (Name \"you\") ++ \"!\"", []))
          ]
    }

-- | A library whose @greeting@ takes a name from now on, which breaks an
-- executable in another package. Both packages have a @src/Main.hs@, so the
-- error can only be placed by the package cabal was building.
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
      changedFiles = ["core/src/Greet.hs", "core/test/HoleyGreetProperties.hs", "core/core.cabal", "app/src/Main.hs"],
      registration = Nothing,
      suiteHeader = "test-suite holey-greet",
      typesArgs = modulesArg "core" "Greet" "newtype Name = Name String",
      functionsArgs = object ["functions" .= [fn "Greet" "greeting" "Name -> String" "Greets the name."]],
      propertiesArgs =
        propertiesArg
          ["import Data.List (isInfixOf)"]
          ""
          "prop_greetingNamesThem"
          "prop_greetingNamesThem :: String -> Bool\nprop_greetingNamesThem n = n `isInfixOf` greeting (Name n)",
      implementations =
        const
          [ ("greeting", ("greeting (Name n) = \"hi \" ++ n", [])),
            ("main", ("main = putStrLn (greeting (Name \"you\") ++ \"!\")", ["import Greet (Name (..))"]))
          ]
    }

packageHeader :: String -> [String]
packageHeader name = ["cabal-version: 2.2", "name: " <> name, "version: 0.1.0.0", "build-type: Simple", ""]

-- | A project of one package, @demo@, at the root.
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
        { llm = scriptedLlm counter (pure . designer layout strategy),
          llmModel = Mercury3,
          toolDefinitions = definitions
        }
      root
      progress
    run <- readRun progress
    cabal <- readFile (root </> cabalPath layout)
    source <- readFile (root </> moduleFile layout)
    published <- readIORef events
    specific <- checks root run source
    pure $
      specific
        ++ concat
          [ expect ("touched files: " <> show (touchedFiles run)) $
              touchedFiles run == changedFiles layout,
            expect "module listed in the component once" $
              maybe True (\line -> length (filter (== line) (lines cabal)) == 1) (registration layout),
            expect "property suite added once" (length (filter (== suiteHeader layout) (lines cabal)) == 1),
            expect "every change published" (case published of RunUpdated latest : _ -> latest == run; _ -> False)
          ]
  pure (map ((label <> ": ") <>) failures)

allPassed :: Run -> [String]
allPassed run =
  concat
    [ expect ("every step passed: " <> show (steps run)) $ all ((== Passed) . snd) (steps run),
      expect ("every function implemented: " <> show (tasks run)) $ map snd (tasks run) == [Done, Done, Done]
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
      expect ("used every iteration: " <> show (iteration run)) (iteration run == maxIterations),
      expect ("alternately reimplemented and redesigned, then gave up: " <> show (history run)) $
        length (history run) == maxIterations
          && and (zipWith T.isInfixOf (cycle ["reimplementing", "redesigning"]) (init (history run)))
          && ("Gave up after " <> T.pack (show maxIterations) <> " iterations") `T.isPrefixOf` last (history run)
    ]

-- | Checks of the finished run and its module that need nothing else.
inspect :: (Run -> String -> [String]) -> FilePath -> Run -> String -> IO [String]
inspect checks _ run source = pure (checks run source)

-- | Passes, and the executable still builds with the module registered.
executableStillBuilds :: FilePath -> Run -> String -> IO [String]
executableStillBuilds root run source = do
  built <- buildTarget root "demo:exe:demo"
  pure (firstAttemptPasses run source ++ expect ("executable builds: " <> either T.unpack (const "") built) (built == Right ()))

-- | Changes @greeting@ in place, adds @farewell@ and @Name@ and exports them,
-- repairs @shout@ while keeping its documentation and signature, and leaves
-- the test suite, which builds with the rest, as it was.
repairsCaller :: FilePath -> Run -> String -> IO [String]
repairsCaller root run source = do
  tests <- readFile (root </> "tests/Main.hs")
  pure $
    allPassed run
      ++ concat
        [ expect ("one iteration: " <> show (history run)) (iteration run == 1 && null (history run)),
          expect ("shout repaired: " <> show (map fst (tasks run))) $
            [(fnName f, fnKind f) | f <- functions (artefact run)] == [(FunctionName "greeting", Designed), (FunctionName "farewell", Designed), (FunctionName "shout", Repair)],
          expect "greeting replaced in place" ("-- | Greets the name.\ngreeting :: Name -> String\ngreeting (Name n) = \"hi \" ++ n" `isInfixOf` source),
          expect "shout keeps its documentation and signature" ("-- | The greeting, loudly.\nshout :: String\nshout = greeting (Name \"you\") ++ \"!\"" `isInfixOf` source),
          expect ("new names exported: " <> takeWhile (/= '\n') source) ("module Demo (greeting, shout, Name (..), farewell) where" `isInfixOf` source),
          expect "existing tests untouched" ("main = if last shout == '!' then exitSuccess else exitFailure" `isInfixOf` tests)
        ]

-- | Repairs @main@ in the other package's @src/Main.hs@, not the library's.
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
          all (`isInfixOf` source) ["import Greet (Name (..))", "-- | Greets loudly.\nmain :: IO ()\nmain = putStrLn (greeting (Name \"you\") ++ \"!\")"],
        expect "the library's executable untouched" ("main = putStrLn \"core\"" `isInfixOf` coreMain)
      ]

-- | Replies for the layout's design, and implementations with @merge@
-- defined by the strategy.
designer :: Layout -> MergeStrategy -> Chat -> Reply
designer layout strategy chat = case latestText chat of
  Just (Tool, _) -> Say "Done."
  _
    | SetTypes `elem` tools' -> CallTool SetTypes (typesArgs layout)
    | SetFunctions `elem` tools' -> CallTool SetFunctions (functionsArgs layout)
    | SetProperties `elem` tools' -> CallTool SetProperties (propertiesArgs layout)
    | ImplementFunction `elem` tools' -> case implementing chat >>= (`lookup` implementations layout (strategy (instructions chat))) of
        Just (definition, extraImports) ->
          CallTool ImplementFunction (object ["definition" .= definition, "imports" .= extraImports])
        Nothing -> Say "I do not know that function."
    | otherwise -> Say "Nothing to do."
  where
    tools' = availableTools chat

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
