{-# LANGUAGE OverloadedStrings #-}

-- | Properties of the pure parts of the server.
module Properties (propertyTests) where

import Core
import Data.List (nub, sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Distribution.ModuleName (fromString)
import Distribution.PackageDescription (GenericPackageDescription (..), Library (..))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Types.CondTree (condTreeData)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
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
      [ check "spans point at each function's signature, definition and imports" prop_spansPointAtFunctions,
        check "an error inside a function's span is attributed to it" prop_errorsAttributedToFunction,
        check "errors outside every function are unattributed" prop_typeErrorsUnattributed,
        check "failed properties are read from suite output" prop_failedPropertiesRead,
        check "a run gives up at the iteration cap" prop_givesUpAtCap,
        check "a broken skeleton is redesigned" prop_skeletonRedesigned,
        check "only known functions are reimplemented" prop_reimplementsKnownFunctions,
        check "repeated property failures are redesigned" prop_repeatedFailureRedesigned,
        check "cabal edits keep the file valid and expose the module and suite" prop_cabalEditsParse,
        check "cabal edits are idempotent" prop_cabalEditsIdempotent
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

word :: Gen Text
word = T.pack <$> listOf1 (elements (['a' .. 'z'] ++ "()=+ "))

paragraph :: Gen Text
paragraph = T.intercalate "\n" <$> resize 4 (listOf word)

importLine :: Gen Text
importLine = ("import Data." <>) . T.toTitle <$> identifier

functionSpec :: Text -> Gen FunctionSpec
functionSpec name = do
  signature <- T.intercalate "\n  -> " <$> resize 3 (listOf1 identifier)
  doc <- paragraph
  body <- oneof [pure Nothing, Just . T.intercalate "\n" . map ((name <> " ") <>) <$> resize 4 (listOf1 word)]
  extra <- resize 2 (listOf importLine)
  pure (FunctionSpec (FunctionName name) signature doc body extra)

newtype AnyArtefact = AnyArtefact Artefact
  deriving (Show)

instance Arbitrary AnyArtefact where
  arbitrary = do
    names <- nub <$> resize 5 (listOf1 identifier)
    fns <- mapM functionSpec names
    exts <- sublistOf ["DerivingStrategies", "LambdaCase"]
    baseImports <- resize 2 (listOf importLine)
    types <- paragraph
    pure (AnyArtefact (Artefact exts baseImports types fns [] "" []))

anyOutcome :: [FunctionName] -> Gen Outcome
anyOutcome names =
  oneof
    [ SkeletonFailed <$> paragraph,
      ImplementationMissing <$> sublistOf names,
      TypeErrors <$> (map (\n -> (n, "error")) <$> sublistOf names) <*> resize 2 (listOf paragraph),
      PropertiesFailed <$> resize 3 (listOf (("prop_" <>) <$> identifier)) <*> sublistOf names <*> paragraph
    ]

functionNames :: Gen [FunctionName]
functionNames = map FunctionName . nub <$> resize 5 (listOf1 identifier)

-- Render and Verify

prop_spansPointAtFunctions :: AnyArtefact -> Property
prop_spansPointAtFunctions (AnyArtefact art) =
  let (source, spans) = renderModule (ModuleName "M") art
      ls = T.lines source
      lineAt n = ls !! (n - 1)
      definitionSpans = [(name, s) | (name, s@(from, _)) <- spans, not ("import " `T.isPrefixOf` lineAt from)]
   in counterexample (T.unpack source) $
        sort (map fst definitionSpans) === sort (map fnName (functions art))
          .&&. conjoin
            [ case [f | f <- functions art, fnName f == name] of
                f : _
                  | "import " `T.isPrefixOf` lineAt from -> property (from == to && lineAt from `elem` fnImports f)
                  | otherwise ->
                      lineAt from === head (T.lines (functionNameText name <> " :: " <> T.strip (fnSignature f)))
                        .&&. lineAt to === last (maybe [functionNameText name <> " = undefined"] (T.lines . T.stripEnd) (fnBody f))
                [] -> property False
            | (name, (from, to)) <- spans
            ]

prop_errorsAttributedToFunction :: AnyArtefact -> Property
prop_errorsAttributedToFunction (AnyArtefact art) =
  let (_, spans) = renderModule (ModuleName "M") art
   in forAll (elements spans) $ \(name, (from, to)) ->
        forAll (chooseInt (from, to)) $ \line ->
          forAll (elements [show line <> ":5", "(" <> show line <> ",5)-(" <> show line <> ",9)"]) $ \location ->
            let output = T.pack ("src/M.hs:" <> location <> ": error: [GHC-83865]\n    • Couldn't match\n   |\n\nother output")
             in fst (attributeErrors "src/M.hs" spans (ghcErrors output)) === [(name, T.pack ("src/M.hs:" <> location <> ": error: [GHC-83865]\n    • Couldn't match\n   |\n") <> "\n")]

prop_typeErrorsUnattributed :: AnyArtefact -> Property
prop_typeErrorsUnattributed (AnyArtefact art) =
  let (_, spans) = renderModule (ModuleName "M") art
      output = "test/Props.hs:3:1: error:\n    bad\n"
   in attributeErrors "src/M.hs" spans (ghcErrors output) === ([], [output])

prop_failedPropertiesRead :: Property
prop_failedPropertiesRead = forAll (resize 5 (listOf ((,) <$> (("prop_" <>) <$> identifier) <*> arbitrary))) $ \results ->
  let output =
        T.unlines
          [ "=== " <> name <> "\n" <> if passed then "+++ OK, passed 100 tests." else "*** Failed! Falsified (after 3 tests):\n1\n2"
          | (name, passed) <- results
          ]
   in failedProperties output === [name | (name, False) <- results]

-- Policy

prop_givesUpAtCap :: Property
prop_givesUpAtCap = forAll functionNames $ \names ->
  forAll (anyOutcome names) $ \outcome ->
    forAll (chooseInt (maxIterations, maxIterations + 3)) $ \i ->
      case decide i names [] outcome of
        GiveUp _ -> True
        _ -> False

prop_skeletonRedesigned :: Property
prop_skeletonRedesigned = forAll functionNames $ \names ->
  forAll (chooseInt (1, maxIterations - 1)) $ \i ->
    decide i names [] (SkeletonFailed "errors") === Redesign "errors"

prop_reimplementsKnownFunctions :: Property
prop_reimplementsKnownFunctions = forAll functionNames $ \names ->
  forAll (anyOutcome names) $ \outcome ->
    forAll (chooseInt (1, maxIterations - 1)) $ \i ->
      case decide i names [] outcome of
        Reimplement work -> counterexample (show work) (all ((`elem` names) . fst) work && not (null work) == not (null names || isMissingNone outcome))
        _ -> property True
  where
    isMissingNone (ImplementationMissing []) = True
    isMissingNone _ = False

prop_repeatedFailureRedesigned :: Property
prop_repeatedFailureRedesigned = forAll functionNames $ \names ->
  forAll (("prop_" <>) <$> identifier) $ \failing ->
    forAll (chooseInt (1, maxIterations - 1)) $ \i ->
      case decide i names [failing] (PropertiesFailed [failing] names "report") of
        Redesign _ -> True
        _ -> False

-- Project

data CabalCase = CabalCase Text [Text] Text
  deriving (Show)

-- | A cabal file with a library in one of several layouts, and a new module.
instance Arbitrary CabalCase where
  arbitrary = do
    indent <- elements ["  ", "    "]
    modules <- map T.toTitle . nub <$> resize 4 (listOf1 identifier)
    new <- (\a b -> "Holey" <> T.toTitle a <> "." <> T.toTitle b) <$> identifier <*> identifier
    layout <- elements [inline, leadingCommas, oneModulePerLine]
    before <- elements ["", "common shared\n  ghc-options: -Wall\n\n"]
    let library =
          T.unlines $
            ["library"]
              ++ map (indent <>) (layout indent modules)
              ++ map (indent <>) ["hs-source-dirs: src", "build-depends: base", "default-language: Haskell2010"]
    pure (CabalCase (header <> before <> library) modules new)
    where
      header = "cabal-version: 2.2\nname: demo\nversion: 0.1.0.0\nbuild-type: Simple\n\n"
      inline _ ms = ["exposed-modules: " <> T.intercalate ", " ms]
      leadingCommas indent ms = "exposed-modules:" : zipWith (\i m -> indent <> (if i == (0 :: Int) then "  " else ", ") <> m) [0 ..] ms
      oneModulePerLine indent ms = "exposed-modules:" : map ((indent <> "  ") <>) ms

demoPackage :: Package
demoPackage = Package "demo" "demo.cabal" "." ["src"] ["base", "containers"]

edited :: CabalCase -> Either Text Text
edited (CabalCase cabal _ new) =
  addTestSuite demoPackage (ModuleName new) <$> addExposedModule (ModuleName new) cabal

prop_cabalEditsParse :: CabalCase -> Property
prop_cabalEditsParse c@(CabalCase _ modules new) = case edited c of
  Left err -> counterexample (T.unpack err) False
  Right text -> counterexample (T.unpack text) $ case parseGenericPackageDescriptionMaybe (TE.encodeUtf8 text) of
    Nothing -> property False
    Just description ->
      let exposed = maybe [] (exposedModules . condTreeData) (condLibrary description)
          suites = map (unUnqualComponentName . fst) (condTestSuites description)
       in sort exposed === sort (map (fromString . T.unpack) (new : modules))
            .&&. suites === [T.unpack (suiteName (ModuleName new))]

prop_cabalEditsIdempotent :: CabalCase -> Property
prop_cabalEditsIdempotent c@(CabalCase _ modules new) =
  let once = edited c
      twice = once >>= \text -> edited (CabalCase text modules new)
   in isJust (either (const Nothing) Just once) ==> once === twice
