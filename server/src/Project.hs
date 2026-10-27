{-# LANGUAGE OverloadedStrings #-}

-- | The user's cabal project: finding library packages, where a run's files
-- go, and the edits a run makes to a package's cabal file.
module Project
  ( Package (..),
    findPackages,
    describePackage,
    validModuleName,
    modulePath,
    propertiesPath,
    suiteName,
    suiteTarget,
    addExposedModule,
    addTestSuite,
  )
where

import Control.Monad (filterM)
import Core (ModuleName (..))
import qualified Data.ByteString as B
import Data.Char (isAlphaNum, isSpace, isUpper, toLower)
import Data.List (isPrefixOf, nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Distribution.Package (depPkgName, pkgName, unPackageName)
import Distribution.PackageDescription (GenericPackageDescription (..), Library (..), package)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (condTreeData)
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (normalise, takeDirectory, takeExtension, (<.>), (</>))

-- | A package with a main library, the only place a run can add a module.
data Package = Package
  { packageName :: Text,
    -- | Relative to the project root, as are the other paths.
    cabalFile :: FilePath,
    packageDir :: FilePath,
    -- | Relative to 'packageDir'.
    sourceDirs :: [FilePath],
    dependencies :: [Text]
  }
  deriving (Eq, Show)

findPackages :: FilePath -> IO [Package]
findPackages root = do
  files <- findCabalFiles root "." (3 :: Int)
  catMaybes <$> mapM (readPackage root) files

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

readPackage :: FilePath -> FilePath -> IO (Maybe Package)
readPackage root file = do
  bytes <- B.readFile (root </> file)
  pure $ do
    description <- parseGenericPackageDescriptionMaybe bytes
    library <- condLibrary description
    let info = libBuildInfo (condTreeData library)
        dirs = map getSymbolicPath (hsSourceDirs info)
    pure
      Package
        { packageName = T.pack (unPackageName (pkgName (package (packageDescription description)))),
          cabalFile = file,
          packageDir = takeDirectory file,
          sourceDirs = if null dirs then ["."] else dirs,
          dependencies = nub [T.pack (unPackageName (depPkgName d)) | d <- targetBuildDepends info]
        }

describePackage :: Package -> Text
describePackage p =
  "- "
    <> packageName p
    <> " (cabal file "
    <> T.pack (cabalFile p)
    <> ", source directories "
    <> T.intercalate ", " [T.pack (normalise (packageDir p </> d)) | d <- sourceDirs p]
    <> ", depends on "
    <> T.intercalate ", " (dependencies p)
    <> ")"

validModuleName :: Text -> Bool
validModuleName name = not (T.null name) && all segment (T.splitOn "." name)
  where
    segment s = case T.uncons s of
      Just (c, rest) -> isUpper c && T.all (\x -> isAlphaNum x || x == '_' || x == '\'') rest
      Nothing -> False

modulePath :: Package -> ModuleName -> FilePath
modulePath p (ModuleName name) =
  normalise (packageDir p </> head (sourceDirs p) </> T.unpack (T.replace "." "/" name) <.> "hs")

propertiesFile :: ModuleName -> FilePath
propertiesFile (ModuleName name) = "Holey" <> T.unpack (T.replace "." "" name) <> "Properties.hs"

propertiesPath :: Package -> ModuleName -> FilePath
propertiesPath p m = normalise (packageDir p </> "test" </> propertiesFile m)

suiteName :: ModuleName -> Text
suiteName (ModuleName name) = "holey-" <> T.map toLower (T.replace "." "-" name)

-- | The cabal target for a run's property suite, e.g. @billing:test:holey-billing-invoice@.
suiteTarget :: Package -> ModuleName -> Text
suiteTarget p m = packageName p <> ":test:" <> suiteName m

-- | Lists the module in the main library's @exposed-modules@ by adding a field
-- line directly under the @library@ header, matching the stanza's indentation.
-- Cabal merges repeated @exposed-modules@ fields.
addExposedModule :: ModuleName -> Text -> Either Text Text
addExposedModule (ModuleName name) cabal =
  case break isLibraryHeader ls of
    (_, []) -> Left "The cabal file has no main library stanza."
    (before, header : after) ->
      let indent = case filter fieldLine after of
            next : _ | leading next > 0 -> T.takeWhile isSpace next
            _ -> "  "
          line = indent <> "exposed-modules: " <> name
       in if line `elem` after
            then Right cabal
            else Right (T.unlines (before ++ header : line : after))
  where
    ls = T.lines cabal
    isLibraryHeader l = T.toLower (T.stripEnd l) == "library"
    fieldLine l = not (T.null (T.strip l)) && not ("--" `T.isPrefixOf` T.strip l)
    leading = T.length . T.takeWhile isSpace

-- | Appends the run's property test suite, unless it is already present.
addTestSuite :: Package -> ModuleName -> Text -> Text
addTestSuite p m cabal
  | header `elem` map T.stripEnd (T.lines cabal) = cabal
  | otherwise =
      T.stripEnd cabal
        <> "\n\n"
        <> T.unlines
          [ header,
            "  type: exitcode-stdio-1.0",
            "  hs-source-dirs: test",
            "  main-is: " <> T.pack (propertiesFile m),
            "  build-depends: " <> T.intercalate ", " deps,
            "  default-language: Haskell2010"
          ]
  where
    header = "test-suite " <> suiteName m
    deps = nub (["base", "QuickCheck", packageName p] ++ dependencies p)
