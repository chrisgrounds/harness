{-# LANGUAGE OverloadedStrings #-}

-- | The user's cabal project: finding the components a run can add a module
-- to, where a run's files go, and the edits a run makes to a cabal file.
module Project
  ( Component (..),
    ComponentKind (..),
    componentName,
    findComponents,
    describeComponent,
    validModuleName,
    modulePath,
    propertiesPath,
    suiteName,
    suiteTarget,
    addModule,
    addTestSuite,
  )
where

import Control.Monad (filterM)
import Core (ModuleName (..))
import qualified Data.ByteString as B
import Data.Char (isAlphaNum, isSpace, isUpper, toLower)
import Data.List (isPrefixOf, nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Package (depPkgName, pkgName, unPackageName)
import Distribution.PackageDescription (GenericPackageDescription (..), package)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty (prettyShow)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (condTreeData)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (normalise, takeDirectory, takeExtension, (<.>), (</>))

-- | Where a run can add a module: a package's main library or one of its
-- executables.
data Component = Component
  { componentPackage :: Text,
    componentKind :: ComponentKind,
    -- | Relative to the project root, as are the other paths.
    cabalFile :: FilePath,
    packageDir :: FilePath,
    -- | Relative to 'packageDir'.
    sourceDirs :: [FilePath],
    dependencies :: [Text],
    -- | The stanza's @default-extensions@ and @default-language@, which an
    -- executable's property suite needs to compile its modules the same way.
    componentExtensions :: [Text],
    componentLanguage :: Maybe Text
  }
  deriving (Eq, Show)

data ComponentKind = Lib | Exe Text
  deriving (Eq, Show)

-- | The component's cabal target, e.g. @billing@ or @billing:exe:billing-cli@,
-- which is also how the model names it.
componentName :: Component -> Text
componentName c = case componentKind c of
  Lib -> componentPackage c
  Exe exe -> componentPackage c <> ":exe:" <> exe

findComponents :: FilePath -> IO [Component]
findComponents root = do
  files <- findCabalFiles root "." (3 :: Int)
  concat <$> mapM (readComponents root) files

findCabalFiles :: FilePath -> FilePath -> Int -> IO [FilePath]
findCabalFiles root dir depth = do
  entries <- filter visible <$> listDirectory (root </> dir)
  let here = [normalise (dir </> e) | e <- entries, takeExtension e == ".cabal"]
  subdirs <-
    if depth <= 0
      then pure []
      else filterM (doesDirectoryExist . (root </>)) [dir </> e | e <- entries]
  nested <- mapM (\d -> findCabalFiles root d (depth - 1)) subdirs
  pure (here ++ concat nested)
  where
    visible name = not ("." `isPrefixOf` name) && name `notElem` ["dist-newstyle", "node_modules"]

readComponents :: FilePath -> FilePath -> IO [Component]
readComponents root file = do
  bytes <- B.readFile (root </> file)
  pure $ case parseGenericPackageDescriptionMaybe bytes of
    Nothing -> []
    Just description ->
      let name = T.pack (unPackageName (pkgName (package (packageDescription description))))
          component kind info =
            let dirs = map getSymbolicPath (hsSourceDirs info)
             in Component
                  { componentPackage = name,
                    componentKind = kind,
                    cabalFile = file,
                    packageDir = takeDirectory file,
                    sourceDirs = if null dirs then ["."] else dirs,
                    dependencies = nub [T.pack (unPackageName (depPkgName d)) | d <- targetBuildDepends info],
                    componentExtensions = map (T.pack . prettyShow) (defaultExtensions info),
                    componentLanguage = T.pack . prettyShow <$> defaultLanguage info
                  }
       in [component Lib (PD.libBuildInfo (condTreeData lib)) | Just lib <- [condLibrary description]]
            ++ [ component (Exe (T.pack (unUnqualComponentName exe))) (PD.buildInfo (condTreeData tree))
               | (exe, tree) <- condExecutables description
               ]

describeComponent :: Component -> Text
describeComponent c =
  "- "
    <> componentName c
    <> " ("
    <> kind
    <> ", cabal file "
    <> T.pack (cabalFile c)
    <> ", source directories "
    <> T.intercalate ", " [T.pack (normalise (packageDir c </> d)) | d <- sourceDirs c]
    <> ", depends on "
    <> T.intercalate ", " (dependencies c)
    <> ")"
  where
    kind = case componentKind c of
      Lib -> "library"
      Exe exe -> "executable " <> exe <> " in package " <> componentPackage c

validModuleName :: Text -> Bool
validModuleName name = not (T.null name) && all segment (T.splitOn "." name)
  where
    segment s = case T.uncons s of
      Just (c, rest) -> isUpper c && T.all (\x -> isAlphaNum x || x == '_' || x == '\'') rest
      Nothing -> False

modulePath :: Component -> ModuleName -> FilePath
modulePath c (ModuleName name) =
  normalise (packageDir c </> head (sourceDirs c) </> T.unpack (T.replace "." "/" name) <.> "hs")

propertiesFile :: ModuleName -> FilePath
propertiesFile (ModuleName name) = "Holey" <> T.unpack (T.replace "." "" name) <> "Properties.hs"

propertiesPath :: Component -> ModuleName -> FilePath
propertiesPath c m = normalise (packageDir c </> "test" </> propertiesFile m)

suiteName :: ModuleName -> Text
suiteName (ModuleName name) = "holey-" <> T.map toLower (T.replace "." "-" name)

-- | The cabal target for a run's property suite, e.g. @billing:test:holey-billing-invoice@.
suiteTarget :: Component -> ModuleName -> Text
suiteTarget c m = componentPackage c <> ":test:" <> suiteName m

-- | Lists the module in the component's stanza: under @exposed-modules@ for a
-- library, @other-modules@ for an executable. Adds a field line directly under
-- the stanza header, matching the stanza's indentation; Cabal merges repeated
-- fields.
addModule :: Component -> ModuleName -> Text -> Either Text Text
addModule c (ModuleName name) cabal =
  case break isHeader ls of
    (_, []) -> Left ("The cabal file has no " <> stanza <> " stanza.")
    (before, header : after) ->
      let body = takeWhile (\l -> not (fieldLine l) || leading l > 0) after
          indent = case filter fieldLine body of
            next : _ -> T.takeWhile isSpace next
            _ -> "  "
          line = indent <> field <> ": " <> name
       in if line `elem` body
            then Right cabal
            else Right (T.unlines (before ++ header : line : after))
  where
    ls = T.lines cabal
    (stanza, field, isHeader) = case componentKind c of
      Lib -> ("main library", "exposed-modules", \l -> T.toLower (T.stripEnd l) == "library")
      Exe exe ->
        ( "executable " <> exe,
          "other-modules",
          \l -> leading l == 0 && case T.words l of
            [keyword, n] -> T.toLower keyword == "executable" && n == exe
            _ -> False
        )
    fieldLine l = not (T.null (T.strip l)) && not ("--" `T.isPrefixOf` T.strip l)
    leading = T.length . T.takeWhile isSpace

-- | Appends the run's property test suite, unless it is already present. A
-- library's suite depends on the package; an executable cannot be depended
-- on, so its suite compiles the module from the executable's source
-- directories, with the executable's dependencies, extensions and language.
addTestSuite :: Component -> ModuleName -> Text -> Text
addTestSuite c m@(ModuleName name) cabal
  | header `elem` map T.stripEnd (T.lines cabal) = cabal
  | otherwise =
      T.stripEnd cabal
        <> "\n\n"
        <> T.unlines (header : map ("  " <>) fields)
  where
    header = "test-suite " <> suiteName m
    fields = case componentKind c of
      Lib ->
        [ "type: exitcode-stdio-1.0",
          "hs-source-dirs: test",
          "main-is: " <> T.pack (propertiesFile m),
          "build-depends: " <> T.intercalate ", " (nub (["base", "QuickCheck", componentPackage c] ++ dependencies c)),
          "default-language: Haskell2010"
        ]
      Exe _ ->
        [ "type: exitcode-stdio-1.0",
          "hs-source-dirs: " <> T.intercalate ", " (nub ("test" : map T.pack (sourceDirs c))),
          "main-is: " <> T.pack (propertiesFile m),
          "other-modules: " <> name,
          "build-depends: " <> T.intercalate ", " (nub (["base", "QuickCheck"] ++ dependencies c)),
          "default-language: " <> fromMaybe "Haskell2010" (componentLanguage c)
        ]
          ++ ["default-extensions: " <> T.intercalate ", " (componentExtensions c) | not (null (componentExtensions c))]
