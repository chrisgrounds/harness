{-# LANGUAGE OverloadedStrings #-}

-- | Properties of the pure parts of the server.
module Properties (propertyTests) where

import Anthropic (replyMessage, requestBody)
import Core
import Data.Aeson (Value (..), decodeStrict, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Foldable (toList)
import Data.List (dropWhileEnd, nub, sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Decls
import Distribution.ModuleName (fromString)
import Distribution.Package (depPkgName, unPackageName)
import Distribution.PackageDescription (BuildInfo (..), Executable (..), GenericPackageDescription (..), Library (..), TestSuite (..))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty (prettyShow)
import Distribution.Types.CondTree (condTreeData)
import Distribution.Types.UnqualComponentName (mkUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Policy
import Project
import Render
import Test.QuickCheck hiding (once, output)
import System.FilePath (takeFileName)
import Verify

-- | Returns failure messages; an empty list means every property held.
propertyTests :: IO [String]
propertyTests =
  concat
    <$> sequence
      [ check "splitting a module and joining it gives it back" prop_splitRoundTrips,
        check "splitting finds every declaration in order" prop_splitFindsDeclarations,
        check "splitting a rendered module finds each function's signature and definition" prop_renderedFunctionsFound,
        check "changing a module keeps every other declaration as it was" prop_changeKeepsOthers,
        check "new names are added to every style of export list" prop_exportsAdded,
        check "a function's equations get a wildcard for each new parameter" prop_equationsAdapted,
        check "a changed function keeps its old equations until implemented" prop_previousBodyKept,
        check "errors are blamed on the declaration they are in" prop_errorsBlamed,
        check "errors record the package being built" prop_errorsRecordPackage,
        check "failed properties are read from suite output" prop_failedPropertiesRead,
        check "a run gives up at the iteration cap" prop_givesUpAtCap,
        check "a broken skeleton is redesigned" prop_skeletonRedesigned,
        check "only known functions are reimplemented" prop_reimplementsKnownFunctions,
        check "repeated property failures are redesigned" prop_repeatedFailureRedesigned,
        check "cabal edits keep the file valid and list the module and its suite" prop_cabalEditsParse,
        check "cabal edits are idempotent" prop_cabalEditsIdempotent,
        check "rewriting the suite replaces it" prop_suiteReplaced,
        check "removing the suite leaves the rest of the cabal file" prop_suiteRemoved,
        check "Claude requests alternate turns, starting with the user" prop_requestsAlternate,
        check "Claude requests answer every tool call in the next turn" prop_toolResultsFollowCalls,
        check "Claude's replies are sent back unchanged" prop_repliesReplayed,
        check "a growing chat keeps its earlier request unchanged" prop_growingChatKeepsPrefix,
        check "Claude replies are read into messages" prop_repliesRead
      ]
  where
    check :: (Testable p) => String -> p -> IO [String]
    check name p = do
      putStr (name <> ": ")
      result <- quickCheckResult p
      pure [name | not (isSuccess result)]

-- Generators

identifier :: Gen Text
identifier = do
  first <- elements ['a' .. 'z']
  rest <- listOf (elements (['a' .. 'z'] ++ ['0' .. '9']))
  pure (T.pack (first : take 8 rest))

-- | Distinct identifiers that are not keywords.
identifiers :: Int -> Gen [Text]
identifiers n = take n . nub . map ("f" <>) <$> vectorOf (n * 3) identifier

word :: Gen Text
word = T.pack <$> listOf1 (elements (['a' .. 'z'] ++ "()=+ "))

paragraph :: Gen Text
paragraph = T.intercalate "\n" <$> resize 4 (listOf word)

importLine :: Gen Text
importLine = ("import Data." <>) . T.toTitle <$> identifier

newModulePath :: FilePath
newModulePath = "src/M.hs"

functionSpec :: FilePath -> Text -> Gen FunctionSpec
functionSpec path name = do
  signature <- T.intercalate "\n  -> " <$> resize 3 (listOf1 identifier)
  doc <- paragraph
  body <- oneof [pure Nothing, Just . T.intercalate "\n" . map ((name <> " ") <>) . map ("= " <>) <$> resize 4 (listOf1 word)]
  extra <- resize 2 (listOf importLine)
  pure (FunctionSpec (FunctionRef path (FunctionName name)) Designed signature doc body Undefined extra Nothing)

-- | A new module and its functions.
data NewModule = NewModule ModulePlan [FunctionSpec]
  deriving (Show)

instance Arbitrary NewModule where
  arbitrary = do
    names <- chooseInt (1, 5) >>= identifiers
    fns <- mapM (functionSpec newModulePath) names
    exts <- sublistOf ["DerivingStrategies", "LambdaCase"]
    baseImports <- resize 2 (listOf importLine)
    types <- elements ["", "data Colour = Red | Green\n  deriving (Eq, Show)", "newtype Count = Count Int"]
    pure (NewModule (ModulePlan "demo" (ModuleName "M") newModulePath True exts baseImports types) fns)

-- | An existing module in one of several layouts, with what it declares.
data Existing = Existing
  { exSource :: Text,
    -- | Each declaration's key and kind, in order.
    exDecls :: [(Text, DeclKind)],
    exHasExports :: Bool
  }
  deriving (Show)

instance Arbitrary Existing where
  arbitrary = do
    count <- chooseInt (2, 6)
    fnNames <- identifiers count
    typeNames <- map (("T" <>) . T.toTitle) <$> identifiers 2
    fns <- mapM existingFunction fnNames
    types <- mapM existingType typeNames
    let instanceDecl = ("instance Show " <> head typeNames, InstanceDecl, "instance Show " <> head typeNames <> " where\n  show _ = \"t\"")
    declsInOrder <- shuffle (fns ++ types ++ [instanceDecl])
    exportStyle <- elements [Nothing, Just Inline, Just Ormolu, Just LeadingCommas]
    imports <- elements [[], ["import Data.List (sort)"], ["import qualified Data.Map as Map", "import Data.Maybe\n  ( fromMaybe,\n  )"]]
    pragmas <- elements [[], ["{-# LANGUAGE LambdaCase #-}"]]
    separators <- vectorOf (length declsInOrder) (elements ["\n", "\n\n", "\n-- Section\n\n"])
    let exported = take 2 [k | (k, ValueDecl, _) <- declsInOrder]
        moduleHeader = case exportStyle of
          Nothing -> ["module Demo where"]
          Just style -> exportList style exported
        headerLines = pragmas ++ ["-- | The module.", ""] ++ moduleHeader ++ [""] ++ imports ++ [""]
        body = T.concat [text <> sep | ((_, _, text), sep) <- zip declsInOrder separators]
    pure (Existing (T.unlines headerLines <> body) [(k, kind) | (k, kind, _) <- declsInOrder] (isJust exportStyle))
    where
      existingFunction name = do
        doc <- elements ["", "-- | Does a thing.\n", "-- | Does a thing,\n-- over two lines.\n"]
        signature <- elements [name <> " :: Int -> Int", name <> " ::\n  Int ->\n  Int"]
        equations <-
          elements
            [ name <> " x = x + 1",
              name <> " 0 = 1\n" <> name <> " n = n",
              name <> " x\n  | x > 0 = y\n  | otherwise = 0\n  where\n    y = 2"
            ]
        pure (name, ValueDecl, doc <> signature <> "\n" <> equations)
      existingType name = do
        text <- elements ["data " <> name <> " = A" <> name <> " | B" <> name <> "\n  deriving (Eq, Show)", "newtype " <> name <> " = " <> name <> " Int"]
        pure (name, DataDecl, text)

data ExportStyle = Inline | Ormolu | LeadingCommas
  deriving (Show)

exportList :: ExportStyle -> [Text] -> [Text]
exportList Inline names = ["module Demo (" <> T.intercalate ", " names <> ") where"]
exportList Ormolu names = ["module Demo", "  ( -- * Things"] ++ ["    " <> n <> "," | n <- names] ++ ["  )", "where"]
exportList LeadingCommas names = ["module Demo"] ++ zipWith (\i n -> (if i == (0 :: Int) then "  ( " else "  , ") <> n) [0 ..] names ++ ["  ) where"]

-- | The names in a module header's export list, read independently of 'Decls'.
exportItems :: Text -> [Text]
exportItems source =
  filter (not . T.null) . map T.strip . splitTop . T.unlines . map dropComment . T.lines . T.drop 1 . T.dropWhile (/= '(') $ before
  where
    before = fst (T.breakOn "where" (snd (T.breakOn "module" source)))
    dropComment l = fst (T.breakOn "--" l)
    splitTop t = go (0 :: Int) "" (T.unpack t)
    go _ acc [] = [T.pack (reverse acc)]
    go depth acc (c : cs)
      | c == '(' = go (depth + 1) (c : acc) cs
      | c == ')' && depth == 0 = [T.pack (reverse acc)]
      | c == ')' = go (depth - 1) (c : acc) cs
      | c == ',' && depth == 0 = T.pack (reverse acc) : go depth "" cs
      | otherwise = go depth (c : acc) cs

-- Decls and Render

prop_splitRoundTrips :: Existing -> NewModule -> Property
prop_splitRoundTrips e (NewModule plan fns) =
  let rendered = renderNewModule plan fns
   in joinSource (splitSource (exSource e)) === exSource e
        .&&. joinSource (splitSource rendered) === rendered

prop_splitFindsDeclarations :: Existing -> Property
prop_splitFindsDeclarations e =
  counterexample (T.unpack (exSource e)) $
    [(k, declKind d) | d <- sourceDecls (splitSource (exSource e)), Just k <- [declKey d]] === exDecls e

prop_renderedFunctionsFound :: NewModule -> Property
prop_renderedFunctionsFound (NewModule plan fns) =
  let source = splitSource (renderNewModule plan fns)
   in conjoin
        [ case findDecl name source of
            Just d ->
              counterexample (show d) $
                declKind d === ValueDecl
                  .&&. any ((name <> " :: ") `T.isPrefixOf`) (declLines d)
                  .&&. last (dropWhileEnd (T.null . T.strip) (declLines d)) === last (maybe [name <> " = undefined"] (T.lines . T.stripEnd) (fnBody f))
            Nothing -> counterexample ("no declaration of " <> T.unpack name) False
        | f <- fns,
          let name = functionNameText (fnName f)
        ]

-- | Replaces one existing function, repairs another, changes one type and
-- adds a function and a type.
prop_changeKeepsOthers :: Existing -> Property
prop_changeKeepsOthers e =
  let fnKeys = [k | (k, ValueDecl) <- exDecls e]
      typeKeys = [k | (k, DataDecl) <- exDecls e]
      replaced = head fnKeys
      repaired = fnKeys !! 1
      changedType = last typeKeys
      path = "src/Demo.hs"
      ref = FunctionRef path . FunctionName
      plan = ModulePlan "demo" (ModuleName "Demo") path False ["TupleSections"] ["import Data.Char (toUpper)"] ("data " <> changedType <> " = Changed\n\ndata TNew = New\n  deriving (Show)")
      fns =
        [ FunctionSpec (ref replaced) Designed "Text -> Text" "Replaced." (Just (replaced <> " t = t")) PreviousBody [] Nothing,
          FunctionSpec (ref "fresh") Designed "Int" "Added." Nothing Undefined ["import Data.Ord (comparing)"] Nothing,
          FunctionSpec (ref repaired) Repair "" "" (Just (repaired <> " _ = 0")) Undefined [] Nothing
        ]
      before = splitSource (exSource e)
      -- A declaration without the blank lines and comments after it.
      code d = dropWhileEnd (\l -> T.null (T.strip l) || "--" `T.isPrefixOf` l) (declLines d)
   in case renderChanged (exSource e) (Just plan) fns of
        Left err -> counterexample (T.unpack err) False
        Right text ->
          let after = splitSource text
              keys = [k | d <- sourceDecls after, Just k <- [declKey d]]
              same k = (code <$> findDecl k after) === (code <$> findDecl k before)
           in counterexample (T.unpack text) $
                keys === map fst (exDecls e) ++ ["TNew", "fresh"]
                  .&&. conjoin [same k | (k, _) <- exDecls e, k `notElem` [replaced, repaired, changedType]]
                  .&&. (code <$> findDecl replaced after) === Just ["-- | Replaced.", replaced <> " :: Text -> Text", replaced <> " t = t"]
                  .&&. (code <$> findDecl changedType after) === Just ["data " <> changedType <> " = Changed"]
                  .&&. ( case (findDecl repaired before, findDecl repaired after) of
                           (Just old, Just new) -> code new === take (declHeadLength old) (declLines old) ++ [repaired <> " _ = 0"]
                           _ -> counterexample "repaired function missing" False
                       )
                  .&&. length (filter (== "import Data.Char (toUpper)") (sourceHeader after)) === 1
                  .&&. length (filter (== "import Data.Ord (comparing)") (sourceHeader after)) === 1
                  .&&. length (filter (== "{-# LANGUAGE TupleSections #-}") (sourceHeader after)) === 1
                  .&&. ( if exHasExports e
                           then
                             counterexample (show (exportItems text)) $
                               all (`elem` exportItems text) ["TNew (..)", "fresh", changedType <> " (..)", replaced]
                                 && nub (map (T.takeWhile (/= ' ')) (exportItems text)) == map (T.takeWhile (/= ' ')) (exportItems text)
                           else property True
                       )

-- | New names are added once; names already exported are left alone.
prop_equationsAdapted :: Existing -> Property
prop_equationsAdapted e =
  conjoin
    [ forAll (elements [0, 1 :: Int]) $ \position ->
        let newSignature = if position == 0 then "Bool -> Int -> Int" else "Int -> Bool -> Int"
            code = dropWhileEnd (\l -> T.null (T.strip l) || "--" `T.isPrefixOf` l) (drop (declHeadLength d) (declLines d))
            expected =
              [ case T.stripPrefix (name <> " ") l of
                  Just rest ->
                    let (pattern, after) = T.breakOn " " rest
                        patterns = if position == 0 then ["_", pattern] else [pattern, "_"]
                     in T.unwords (name : patterns) <> after
                  Nothing -> l
              | l <- code
              ]
         in counterexample (T.unpack (T.unlines (declLines d))) $
              adaptEquations name newSignature d === Just expected
                .&&. adaptEquations name "Int -> Int" d === Just code
                .&&. adaptEquations name "Int" d === Nothing
    | d <- sourceDecls (splitSource (exSource e)),
      declKind d == ValueDecl,
      Just name <- [declKey d]
    ]

prop_previousBodyKept :: Property
prop_previousBodyKept =
  let source = "module Demo (greeting) where\n\n-- | How to greet.\ngreeting :: String\ngreeting = \"hi\"\n"
      spec placeholder = FunctionSpec (FunctionRef "src/Demo.hs" (FunctionName "greeting")) Designed "Name -> String" "Greets the name." Nothing placeholder [] Nothing
      rendered placeholder = renderChanged source Nothing [spec placeholder]
   in rendered PreviousBody === Right "module Demo (greeting) where\n\n-- | Greets the name.\ngreeting :: Name -> String\ngreeting _ = \"hi\"\n"
        .&&. rendered Undefined === Right "module Demo (greeting) where\n\n-- | Greets the name.\ngreeting :: Name -> String\ngreeting = undefined\n"

prop_exportsAdded :: Property
prop_exportsAdded =
  forAll (elements [Inline, Ormolu, LeadingCommas]) $ \style ->
    forAll (chooseInt (1, 3) >>= identifiers) $ \old ->
      forAll (chooseInt (1, 2) >>= identifiers) $ \new ->
        forAll (sublistOf old) $ \repeated ->
          let source = T.unlines (exportList style old ++ ["", "x :: Int", "x = 1"])
              added = map ("new" <>) new
              text = joinSource (addExports (repeated ++ added ++ added) (splitSource source))
           in counterexample (T.unpack text) $
                exportItems text === old ++ added
                  .&&. exportedNames (splitSource text) === Just (old ++ added)
                  .&&. sourceDecls (splitSource text) === sourceDecls (splitSource source)

prop_errorsBlamed :: Existing -> Property
prop_errorsBlamed e =
  let path = "src/Demo.hs"
      source = splitSource (exSource e)
      fnKeys = [k | (k, ValueDecl) <- exDecls e]
      planned = take 1 fnKeys
      fns = [FunctionSpec (FunctionRef path (FunctionName k)) Designed "" "" Nothing Undefined [] Nothing | k <- planned]
   in conjoin
        [ forAll (chooseInt (from, to)) $ \line ->
            counterexample (show (declKey d, line)) $
              blame fns path source line === case (declKind d, declKey d) of
                (ValueDecl, Just k)
                  | k `elem` planned -> InPlanned (FunctionRef path (FunctionName k))
                  | otherwise -> InOther (FunctionRef path (FunctionName k))
                _ -> Unblamed
        | (d, (from, to)) <- declSpans source
        ]

prop_errorsRecordPackage :: Property
prop_errorsRecordPackage =
  forAll (elements ["core", "hs-tui-client", "demo2"]) $ \package ->
    forAll (elements ["Building library for ", "Building executable 'app' for ", "Building test suite 'tests' for ", "Preprocessing library for "]) $ \prefix ->
      let output = prefix <> package <> "-0.1.0.0..\n[1 of 2] Compiling M\nsrc/M.hs:3:5: error: [GHC-83865]\n    bad\n"
       in map errorPackage (ghcErrors output) === [Just package]

prop_failedPropertiesRead :: Property
prop_failedPropertiesRead = forAll (resize 5 (listOf ((,) <$> (("prop_" <>) <$> identifier) <*> arbitrary))) $ \results ->
  let output =
        T.unlines
          [ "=== " <> name <> "\n" <> if passed then "+++ OK, passed 100 tests." else "*** Failed! Falsified (after 3 tests):\n1\n2"
          | (name, passed) <- results
          ]
   in failedProperties output === [name | (name, False) <- results]

-- Policy

functionRefs :: Gen [FunctionRef]
functionRefs = do
  names <- chooseInt (1, 5) >>= identifiers
  mapM (\n -> FunctionRef <$> elements ["src/A.hs", "app/Main.hs"] <*> pure (FunctionName n)) names

anyOutcome :: [FunctionRef] -> Gen Outcome
anyOutcome refs =
  oneof
    [ SkeletonFailed <$> paragraph,
      ImplementationMissing <$> sublistOf refs,
      TypeErrors <$> (map (\n -> (n, "error")) <$> sublistOf refs) <*> resize 2 (listOf paragraph),
      PropertiesFailed <$> resize 3 (listOf (("prop_" <>) <$> identifier)) <*> sublistOf refs <*> paragraph
    ]

prop_givesUpAtCap :: Property
prop_givesUpAtCap = forAll functionRefs $ \refs ->
  forAll (anyOutcome refs) $ \outcome ->
    forAll (chooseInt (maxIterations, maxIterations + 3)) $ \i ->
      case decide i refs [] outcome of
        GiveUp _ -> True
        _ -> False

prop_skeletonRedesigned :: Property
prop_skeletonRedesigned = forAll functionRefs $ \refs ->
  forAll (chooseInt (1, maxIterations - 1)) $ \i ->
    decide i refs [] (SkeletonFailed "errors") === Redesign "errors"

prop_reimplementsKnownFunctions :: Property
prop_reimplementsKnownFunctions = forAll functionRefs $ \refs ->
  forAll (anyOutcome refs) $ \outcome ->
    forAll (chooseInt (1, maxIterations - 1)) $ \i ->
      case decide i refs [] outcome of
        Reimplement work -> counterexample (show work) (all ((`elem` refs) . fst) work && not (null work) == not (null refs || isMissingNone outcome))
        _ -> property True
  where
    isMissingNone (ImplementationMissing []) = True
    isMissingNone _ = False

prop_repeatedFailureRedesigned :: Property
prop_repeatedFailureRedesigned = forAll functionRefs $ \refs ->
  forAll (("prop_" <>) <$> identifier) $ \failing ->
    forAll (chooseInt (1, maxIterations - 1)) $ \i ->
      case decide i refs [failing] (PropertiesFailed [failing] refs "report") of
        Redesign _ -> True
        _ -> False

-- Project

data CabalCase = CabalCase
  { caseComponent :: Component,
    caseCabal :: Text,
    caseModules :: [Text],
    caseNew :: Text
  }
  deriving (Show)

-- | A cabal file whose library or executable lists its modules in one of
-- several layouts, sometimes followed by a stanza that already lists the new
-- module, and a new module for it.
instance Arbitrary CabalCase where
  arbitrary = do
    modules' <- map T.toTitle . nub <$> resize 4 (listOf1 identifier)
    component <- elements [library, executable modules']
    indent <- elements ["  ", "    "]
    new <- (\a b -> "Holey" <> T.toTitle a <> "." <> T.toTitle b) <$> identifier <*> identifier
    layout <- elements [inline, leadingCommas, oneModulePerLine]
    before <- elements ["", "common shared\n  ghc-options: -Wall\n\n"]
    after <- elements ["", "\nbenchmark other\n" <> indent <> "type: exitcode-stdio-1.0\n" <> indent <> "main-is: Bench.hs\n" <> indent <> "other-modules: " <> new <> "\n" <> indent <> "build-depends: base\n" <> indent <> "default-language: Haskell2010\n"]
    let (header, field, extra) = case componentKind component of
          Lib -> ("library", "exposed-modules", ["hs-source-dirs: src"])
          Exe exe -> ("executable " <> exe, "other-modules", ["main-is: Main.hs", "hs-source-dirs: app"])
        stanza =
          T.unlines $
            [header]
              ++ map (indent <>) (layout indent field modules')
              ++ map (indent <>) (extra ++ ["build-depends: base, containers", "default-language: Haskell2010"])
    pure (CabalCase component (preamble <> before <> stanza <> after) modules' new)
    where
      preamble = "cabal-version: 2.2\nname: demo\nversion: 0.1.0.0\nbuild-type: Simple\n\n"
      inline _ field ms = [field <> ": " <> T.intercalate ", " ms]
      leadingCommas indent field ms = (field <> ":") : zipWith (\i m -> indent <> (if i == (0 :: Int) then "  " else ", ") <> m) [0 ..] ms
      oneModulePerLine indent field ms = (field <> ":") : map ((indent <> "  ") <>) ms
      library = Component "demo" Lib "demo.cabal" "." ["src"] ["base", "containers"] [] Nothing [] []
      executable ms = Component "demo" (Exe "app") "demo.cabal" "." ["app"] ["base", "containers"] ["OverloadedStrings"] (Just "Haskell2010") ms []

edited :: CabalCase -> Either Text Text
edited c =
  upsertTestSuite (caseComponent c) (ModuleName (caseNew c)) [ModuleName (caseNew c)] <$> addModule (caseComponent c) (ModuleName (caseNew c)) (caseCabal c)

-- | The component lists its old modules and the new one; the suite runs the
-- new module's properties, depending on the library or, for an executable,
-- compiling its modules from its source directories.
prop_cabalEditsParse :: CabalCase -> Property
prop_cabalEditsParse c = case edited c of
  Left err -> counterexample (T.unpack err) False
  Right text -> counterexample (T.unpack text) $ case parseGenericPackageDescriptionMaybe (TE.encodeUtf8 text) of
    Nothing -> property False
    Just description ->
      let expected = sort (map (fromString . T.unpack) (caseNew c : caseModules c))
          listed = case componentKind (caseComponent c) of
            Lib -> maybe [] (exposedModules . condTreeData) (condLibrary description)
            Exe exe -> maybe [] (otherModules . buildInfo . condTreeData) (lookup (mkUnqualComponentName (T.unpack exe)) (condExecutables description))
          suite = lookup (mkUnqualComponentName (T.unpack (suiteName (ModuleName (caseNew c))))) (condTestSuites description)
          suiteInfo = testBuildInfo . condTreeData <$> suite
          depends = maybe [] (map (unPackageName . depPkgName) . targetBuildDepends) suiteInfo
       in sort listed === expected
            .&&. case (componentKind (caseComponent c), suiteInfo) of
              (_, Nothing) -> counterexample "no property suite" False
              (Lib, Just _) -> property ("demo" `elem` depends)
              (Exe _, Just info) ->
                sort (otherModules info) === expected
                  .&&. map getSymbolicPath (hsSourceDirs info) === ["test", "app"]
                  .&&. "demo" `notElem` depends
                  .&&. map prettyShow (defaultExtensions info) === ["OverloadedStrings"]

prop_cabalEditsIdempotent :: CabalCase -> Property
prop_cabalEditsIdempotent c =
  let once = edited c
      twice = once >>= \text -> edited c {caseCabal = text}
   in isJust (either (const Nothing) Just once) ==> once === twice

-- | Writing the suite again with other modules leaves one stanza, listing them.
prop_suiteReplaced :: CabalCase -> Property
prop_suiteReplaced c =
  let main = ModuleName (caseNew c)
      first = upsertTestSuite (caseComponent c) main [main] (caseCabal c)
      second = upsertTestSuite (caseComponent c) main [ModuleName "Other.Module"] first
      header = "test-suite " <> suiteName main
   in counterexample (T.unpack second) $
        length (filter ((== header) . T.stripEnd) (T.lines second)) === 1
          .&&. case componentKind (caseComponent c) of
            Exe _ -> property ("Other.Module" `T.isInfixOf` second && not (("other-modules: " <> T.intercalate ", " (caseModules c ++ [caseNew c])) `T.isInfixOf` second))
            Lib -> second === upsertTestSuite (caseComponent c) main [] (caseCabal c)

-- | A suite written and then removed leaves the cabal file's components as they were.
prop_suiteRemoved :: CabalCase -> Property
prop_suiteRemoved c =
  let main = ModuleName (caseNew c)
      file = takeFileName (propertiesPath (caseComponent c) main)
      removed = removeTestSuite file (upsertTestSuite (caseComponent c) main [main] (caseCabal c))
      stanzaNames description = (fmap (const ()) (condLibrary description), map fst (condExecutables description), map fst (condTestSuites description), map fst (condBenchmarks description))
   in counterexample (T.unpack removed) $
        not ("test-suite holey-" `T.isInfixOf` removed)
          .&&. (stanzaNames <$> parseGenericPackageDescriptionMaybe (TE.encodeUtf8 removed)) === (stanzaNames <$> parseGenericPackageDescriptionMaybe (TE.encodeUtf8 (caseCabal c)))

-- Anthropic

-- | An agent conversation as the harness builds one, oldest first: a system
-- prompt and instruction, then rounds of a reply (sometimes kept as the
-- API's content blocks), its tool results, and sometimes a nudge.
newtype History = History [Message]
  deriving (Show)

instance Arbitrary History where
  arbitrary = do
    rounds <- chooseInt (0, 4)
    History . ([msg System "You are holey.", msg User "Step 1: types."] ++) . concat <$> mapM reply [1 .. rounds]
    where
      msg r text = Message r (Just text) [] Nothing Nothing Nothing
      reply :: Int -> Gen [Message]
      reply n = do
        calls <- chooseInt (0, 2)
        names <- vectorOf calls (elements [minBound .. maxBound])
        text <- elements ["", "Reading the code.", "Done."]
        raw <- arbitrary
        let requests = [ToolRequest ("toolu_" <> T.pack (show n <> "_" <> show i)) FunctionTool (ToolRequestFunctionParams name "{\"path\":\"src\"}") | (i, name) <- zip [0 :: Int ..] names]
            blocks =
              [object ["type" .= ("thinking" :: Text), "thinking" .= ("" :: Text), "signature" .= ("sig" <> T.pack (show n))]]
                ++ [object ["type" .= ("text" :: Text), "text" .= text] | not (T.null text)]
                ++ [object ["type" .= ("tool_use" :: Text), "id" .= toolRequestId r, "name" .= toolNameText (toolRequestFunctionParamsName (toolRequestFunction r)), "input" .= object ["path" .= ("src" :: Text)]] | r <- requests]
            assistant = Message Assistant (Just text) requests Nothing Nothing (if raw then Just (toJSON blocks) else Nothing)
            results = [Message Tool (Just "a.hs") [] (Just (toolRequestId r)) Nothing Nothing | r <- requests]
        nudge <- elements [[], [msg User "Call the tool."]]
        pure ([assistant] ++ results ++ if null requests then [msg User "Go on."] else nudge)

chatOf :: Model -> [Message] -> Chat
chatOf m ms = Chat (reverse ms) m []

turns :: Chat -> [(Text, [Value])]
turns chat = case requestBody chat of
  Object o | Just (Array ts) <- KeyMap.lookup "messages" o -> [(r, toList bs) | Object t <- toList ts, Just (String r) <- [KeyMap.lookup "role" t], Just (Array bs) <- [KeyMap.lookup "content" t]]
  _ -> []

jsonField :: Text -> Value -> Maybe Value
jsonField key (Object o) = KeyMap.lookup (Key.fromText key) o
jsonField _ _ = Nothing

prop_requestsAlternate :: History -> Property
prop_requestsAlternate (History ms) =
  let ts = turns (chatOf ClaudeOpus ms)
   in counterexample (show ts) $
        map fst ts === take (length ts) (cycle ["user", "assistant"])
          .&&. all (not . null . snd) ts
          .&&. jsonField "system" (requestBody (chatOf ClaudeOpus ms)) === Just (String "You are holey.")

prop_toolResultsFollowCalls :: History -> Property
prop_toolResultsFollowCalls (History ms) =
  let ts = turns (chatOf ClaudeSonnet ms)
      ids kind key blocks = [i | b <- blocks, jsonField "type" b == Just (String kind), Just (String i) <- [jsonField key b]]
   in conjoin
        [ ids "tool_use" "id" assistant === take (length (ids "tool_use" "id" assistant)) (ids "tool_result" "tool_use_id" next)
        | (("assistant", assistant), (_, next)) <- zip ts (drop 1 ts)
        ]

prop_repliesReplayed :: History -> Property
prop_repliesReplayed (History ms) =
  let raw = [toList blocks | m <- ms, role m == Assistant, Just (Array blocks) <- [providerContent m]]
      -- Only replies kept as the API sent them have thinking blocks.
      replayed = [b | ("assistant", b) <- turns (chatOf ClaudeHaiku ms), any ((== Just (String "thinking")) . jsonField "type") b]
   in replayed === raw

prop_growingChatKeepsPrefix :: History -> History -> Property
prop_growingChatKeepsPrefix (History ms) (History more) =
  let extra = drop 2 more
      before = turns (chatOf ClaudeOpus ms)
      after = turns (chatOf ClaudeOpus (ms ++ extra))
      settled = take (length before - 1) before
   in counterexample (show (before, after)) $
        take (length settled) after === settled
          .&&. jsonField "system" (requestBody (chatOf ClaudeOpus ms)) === jsonField "system" (requestBody (chatOf ClaudeOpus (ms ++ extra)))

prop_repliesRead :: Property
prop_repliesRead =
  forAll (sublistOf [minBound .. maxBound :: ToolName]) $ \names ->
    forAll (elements ["end_turn", "tool_use", "refusal"]) $ \stop ->
      let blocks =
            [object ["type" .= ("thinking" :: Text), "thinking" .= ("" :: Text), "signature" .= ("s" :: Text)], object ["type" .= ("text" :: Text), "text" .= ("Looking." :: Text)]]
              ++ [object ["type" .= ("tool_use" :: Text), "id" .= ("toolu_" <> toolNameText n), "name" .= toolNameText n, "input" .= object ["path" .= toolNameText n]] | n <- names]
          response = object ["content" .= blocks, "stop_reason" .= (stop :: Text), "usage" .= object ["output_tokens" .= (7 :: Int)]]
       in case parseEither replyMessage response of
            Left err -> counterexample err False
            Right (m, tokens) ->
              tokens === 7
                .&&. if stop == "refusal"
                  then toolCalls m === [] .&&. providerContent m === Nothing
                  else
                    content m === Just "Looking."
                      .&&. map (toolRequestFunctionParamsName . toolRequestFunction) (toolCalls m) === names
                      .&&. [decodeStrict (TE.encodeUtf8 (toolRequestFunctionParamsArguments (toolRequestFunction r))) | r <- toolCalls m] === [Just (object ["path" .= toolNameText n]) | n <- names]
                      .&&. providerContent m === Just (toJSON blocks)
