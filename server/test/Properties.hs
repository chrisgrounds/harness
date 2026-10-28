{-# LANGUAGE OverloadedStrings #-}

-- | Properties of the pure parts of the server.
module Properties (propertyTests) where

import Core
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
        check "errors are blamed on the declaration they are in" prop_errorsBlamed,
        check "errors record the package being built" prop_errorsRecordPackage,
        check "failed properties are read from suite output" prop_failedPropertiesRead,
        check "a run gives up at the iteration cap" prop_givesUpAtCap,
        check "a broken skeleton is redesigned" prop_skeletonRedesigned,
        check "only known functions are reimplemented" prop_reimplementsKnownFunctions,
        check "repeated property failures are redesigned" prop_repeatedFailureRedesigned,
        check "cabal edits keep the file valid and list the module and its suite" prop_cabalEditsParse,
        check "cabal edits are idempotent" prop_cabalEditsIdempotent,
        check "rewriting the suite replaces it" prop_suiteReplaced
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
  pure (FunctionSpec (FunctionRef path (FunctionName name)) Designed signature doc body extra Nothing)

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

-- | The names in a module header's export list.
exportedNames :: Text -> [Text]
exportedNames source =
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
        [ FunctionSpec (ref replaced) Designed "Text -> Text" "Replaced." (Just (replaced <> " t = t")) [] Nothing,
          FunctionSpec (ref "fresh") Designed "Int" "Added." Nothing ["import Data.Ord (comparing)"] Nothing,
          FunctionSpec (ref repaired) Repair "" "" (Just (repaired <> " _ = 0")) [] Nothing
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
                  .&&. (if exHasExports e then conjoin [counterexample (show (exportedNames text)) (name `elem` exportedNames text) | name <- ["TNew (..)", "fresh"]] else property True)

prop_exportsAdded :: Property
prop_exportsAdded =
  forAll (elements [Inline, Ormolu, LeadingCommas]) $ \style ->
    forAll (chooseInt (1, 3) >>= identifiers) $ \old ->
      forAll (chooseInt (1, 2) >>= identifiers) $ \new ->
        let source = T.unlines (exportList style old ++ ["", "x :: Int", "x = 1"])
            text = joinSource (addExports (map ("new" <>) new) (splitSource source))
         in counterexample (T.unpack text) $
              exportedNames text === old ++ map ("new" <>) new
                .&&. sourceDecls (splitSource text) === sourceDecls (splitSource source)

prop_errorsBlamed :: Existing -> Property
prop_errorsBlamed e =
  let path = "src/Demo.hs"
      source = splitSource (exSource e)
      fnKeys = [k | (k, ValueDecl) <- exDecls e]
      planned = take 1 fnKeys
      fns = [FunctionSpec (FunctionRef path (FunctionName k)) Designed "" "" Nothing [] Nothing | k <- planned]
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
