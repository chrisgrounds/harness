{-# LANGUAGE OverloadedStrings #-}

module Project
  ( ProjectInfo (..),
    Component (..),
    ComponentKind (..),
    componentName,
    findProject,
    locateModule,
    sourceFiles,
    describeComponent,
    validModuleName,
    modulePath,
    propertiesPath,
    suiteName,
    suiteTarget,
    addModule,
    upsertTestSuite,
    removeTestSuite,
    visibleFrom,
  )
where

import Control.Monad (filterM, forM)
import Core (ComponentName (..), ModuleName (..))
import qualified Data.ByteString as B
import Data.Char (isSpace, isUpper, toLower)
import Decls (isIdentChar)
import Data.List (find, isPrefixOf, nub)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Distribution.Fields (Field (..), FieldLine (..), Name (..), SectionArg (..), readFields)
import Distribution.Package (depPkgName, pkgName, unPackageName)
import Distribution.Parsec.Position (Position (..))
import Distribution.PackageDescription (GenericPackageDescription (..), package)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Pretty (prettyShow)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (condTreeData)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (normalise, takeDirectory, takeExtension, (<.>), (</>))

data ProjectInfo = ProjectInfo
  { components :: [Component],
    packages :: [(Text, FilePath)]
  }

data Component = Component
  { componentPackage :: Text,
    componentKind :: ComponentKind,
    cabalFile :: FilePath,
    packageDir :: FilePath,
    sourceDirs :: [FilePath],
    dependencies :: [Text],
    componentExtensions :: [Text],
    componentLanguage :: Maybe Text,
    componentModules :: [Text],
    componentAutogen :: [Text]
  }
  deriving (Eq, Show)

data ComponentKind = Lib | Exe Text
  deriving (Eq, Show)

componentName :: Component -> ComponentName
componentName c = ComponentName $ case componentKind c of
  Lib -> componentPackage c
  Exe exe -> componentPackage c <> ":exe:" <> exe

findProject :: FilePath -> IO ProjectInfo
findProject root = do
  files <- findCabalFiles root "." (3 :: Int)
  found <- mapM (readPackage root) files
  pure
    ProjectInfo
      { components = concat [cs | Just (_, _, cs) <- found],
        packages = [(name, dir) | Just (name, dir, _) <- found]
      }

locateModule :: FilePath -> Component -> ModuleName -> IO (Maybe FilePath)
locateModule root c (ModuleName name) = do
  let candidates = [normalise (packageDir c </> d </> T.unpack (T.replace "." "/" name) <.> "hs") | d <- sourceDirs c]
  existing <- filterM (doesFileExist . (root </>)) candidates
  pure (listToMaybe existing)


sourceFiles :: FilePath -> [Component] -> IO [FilePath]
sourceFiles root cs = nub . concat <$> mapM walk (nub [normalise (packageDir c </> d) | c <- cs, d <- sourceDirs c])
  where
    walk dir = do
      exists <- doesDirectoryExist (root </> dir)
      entries <- if exists then filter (not . ("." `isPrefixOf`)) <$> listDirectory (root </> dir) else pure []
      fmap concat . forM entries $ \e -> do
        let path = dir </> e
        isDir <- doesDirectoryExist (root </> path)
        if isDir then walk path else pure [path | takeExtension e == ".hs"]

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

readPackage :: FilePath -> FilePath -> IO (Maybe (Text, FilePath, [Component]))
readPackage root file = do
  bytes <- B.readFile (root </> file)
  pure $ case parseGenericPackageDescriptionMaybe bytes of
    Nothing -> Nothing
    Just description -> Just $
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
                    componentLanguage = T.pack . prettyShow <$> defaultLanguage info,
                    componentModules = map (T.pack . prettyShow) (otherModules info),
                    componentAutogen = map (T.pack . prettyShow) (autogenModules info)
                  }
       in ( name,
            takeDirectory file,
            [component Lib (PD.libBuildInfo (condTreeData lib)) | Just lib <- [condLibrary description]]
              ++ [ component (Exe (T.pack (unUnqualComponentName exe))) (PD.buildInfo (condTreeData tree))
                 | (exe, tree) <- condExecutables description
                 ]
          )

describeComponent :: Component -> Text
describeComponent c =
  "- "
    <> componentNameText (componentName c)
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
      Just (c, rest) -> isUpper c && T.all isIdentChar rest
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

suiteTarget :: Component -> ModuleName -> Text
suiteTarget c m = componentPackage c <> ":test:" <> suiteName m

visibleFrom :: [Component] -> Component -> ComponentName -> Bool
visibleFrom cs suite owner = owner == componentName suite || owner `elem` libraries
  where
    depends = [componentPackage suite | componentKind suite == Lib] ++ dependencies suite
    libraries = [componentName c | c <- cs, componentKind c == Lib, componentPackage c `elem` depends]

addModule :: Component -> ModuleName -> Text -> Either Text Text
addModule c (ModuleName name) cabal = do
  all' <- maybe (Left "The cabal file could not be read.") Right (stanzas cabal)
  s <- maybe (Left ("The cabal file has no " <> describe <> " stanza.")) Right (find matches all')
  pure $
    if name `elem` concat [fieldValues ls | Field (Name _ n) ls <- stanzaFields s, lower n == field]
      then cabal
      else insertAfter (afterImports s) (indent s <> field <> ": " <> name) cabal
  where
    afterImports s = maximum (fst (stanzaLines s) : concatMap fieldRows (takeWhile isImport (stanzaFields s)))
    isImport (Field (Name _ n) _) = lower n == "import"
    isImport (Section _ _ _) = False
    (describe, field, matches) = case componentKind c of
      Lib -> ("main library", "exposed-modules", \s -> stanzaKeyword s == "library" && null (stanzaArgs s))
      Exe exe -> ("executable " <> exe, "other-modules", \s -> stanzaKeyword s == "executable" && stanzaArgs s == [exe])
    indent s = case [column | Field (Name (Position _ column) _) _ <- stanzaFields s] of
      column : _ -> T.replicate (column - 1) " "
      [] -> "  "

upsertTestSuite :: Component -> ModuleName -> [ModuleName] -> Text -> Text
upsertTestSuite c m newModules cabal =
  T.stripEnd (withoutStanzas (\s -> stanzaKeyword s == "test-suite" && stanzaArgs s == [suiteName m]) cabal)
    <> "\n\n"
    <> T.unlines (header : map ("  " <>) fields)
  where
    header = "test-suite " <> suiteName m
    modulesListed = nub (componentModules c ++ map moduleNameText newModules)
    fields = case componentKind c of
      Lib ->
        [ "type: exitcode-stdio-1.0",
          "hs-source-dirs: test",
          "main-is: " <> T.pack (propertiesFile m),
          "build-depends: " <> T.intercalate ", " (nub (["base", "QuickCheck", componentPackage c] ++ dependencies c)),
          "default-language: Haskell2010",
          "ghc-options: -Wall"
        ]
      Exe _ ->
        [ "type: exitcode-stdio-1.0",
          "hs-source-dirs: " <> T.intercalate ", " (nub ("test" : map T.pack (sourceDirs c))),
          "main-is: " <> T.pack (propertiesFile m),
          "build-depends: " <> T.intercalate ", " (nub (["base", "QuickCheck"] ++ dependencies c)),
          "default-language: " <> fromMaybe "Haskell2010" (componentLanguage c),
          "ghc-options: -Wall"
        ]
          ++ ["other-modules: " <> T.intercalate ", " modulesListed | not (null modulesListed)]
          ++ ["autogen-modules: " <> T.intercalate ", " (componentAutogen c) | not (null (componentAutogen c))]
          ++ ["default-extensions: " <> T.intercalate ", " (componentExtensions c) | not (null (componentExtensions c))]

removeTestSuite :: FilePath -> Text -> Text
removeTestSuite file = withoutStanzas runs
  where
    runs s =
      stanzaKeyword s == "test-suite"
        && any ("holey-" `T.isPrefixOf`) (stanzaArgs s)
        && T.pack file `elem` concat [fieldValues ls | Field (Name _ n) ls <- stanzaFields s, lower n == "main-is"]

data Stanza = Stanza
  { stanzaKeyword :: Text,
    stanzaArgs :: [Text],
    stanzaFields :: [Field Position],
    stanzaLines :: (Int, Int)
  }

stanzas :: Text -> Maybe [Stanza]
stanzas cabal = either (const Nothing) (Just . mapMaybe stanza) (readFields (TE.encodeUtf8 cabal))
  where
    stanza (Section (Name (Position row _) keyword) args fields) =
      Just (Stanza (lower keyword) (map argument args) fields (row, maximum (row : concatMap fieldRows fields)))
    stanza (Field _ _) = Nothing
    argument (SecArgName _ a) = TE.decodeUtf8 a
    argument (SecArgStr _ a) = TE.decodeUtf8 a
    argument (SecArgOther _ a) = TE.decodeUtf8 a

fieldRows :: Field Position -> [Int]
fieldRows (Field (Name (Position row _) _) ls) = row : [r | FieldLine (Position r _) _ <- ls]
fieldRows (Section (Name (Position row _) _) _ fields) = row : concatMap fieldRows fields

fieldValues :: [FieldLine Position] -> [Text]
fieldValues ls = filter (not . T.null) (T.split (\ch -> ch == ',' || isSpace ch) (T.unwords [TE.decodeUtf8 l | FieldLine _ l <- ls]))

lower :: B.ByteString -> Text
lower = T.toLower . TE.decodeUtf8

insertAfter :: Int -> Text -> Text -> Text
insertAfter row line cabal = let ls = T.lines cabal in T.unlines (take row ls ++ [line] ++ drop row ls)

withoutStanzas :: (Stanza -> Bool) -> Text -> Text
withoutStanzas gone cabal = case stanzas cabal of
  Nothing -> cabal
  Just all' ->
    let ls = T.lines cabal
        ranges = [stanzaLines s | s <- all', gone s]
        removed i = any (\(from, to) -> i >= from && i <= to + length (takeWhile (T.null . T.strip) (drop to ls))) ranges
     in T.unlines [l | (i, l) <- zip [1 ..] ls, not (removed i)]
