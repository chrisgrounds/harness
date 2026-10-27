{-# LANGUAGE OverloadedStrings #-}

-- | Writing a run's artefact into the user's project.
module Workspace (Target (..), resolveTarget, existingFiles, writeArtefact, uncommittedChanges) where

import Control.Exception (IOException, try)
import Core
import qualified Data.ByteString as B
import Data.List (find, nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Project
import Render (Span, renderModule, renderProperties)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

-- | Where a run's code lives, once step 1 has chosen it.
data Target = Target
  { targetPkg :: Package,
    targetName :: ModuleName,
    targetModulePath :: FilePath,
    targetPropertiesPath :: FilePath,
    targetSuite :: Text
  }

resolveTarget :: [Package] -> Run -> Maybe Target
resolveTarget packages run = do
  name <- targetModule run
  pkgName <- targetPackage run
  pkg <- find ((== pkgName) . packageName) packages
  pure (Target pkg name (modulePath pkg name) (propertiesPath pkg name) (suiteTarget pkg name))

-- | Writes the module and property suite and registers both in the cabal
-- file. Refuses to overwrite files the run did not create. Returns the files
-- touched and where each function is in the module.
writeArtefact :: FilePath -> Target -> Run -> IO (Either Text ([FilePath], [(FunctionName, Span)]))
writeArtefact root target run = do
  clashes <- existingFiles root run [targetModulePath target, targetPropertiesPath target]
  if not (null clashes)
    then pure (Left ("Refusing to overwrite existing files: " <> T.intercalate ", " (map T.pack clashes)))
    else do
      let (moduleSource, spans) = renderModule (targetName target) (artefact run)
          cabal = cabalFile (targetPkg target)
      writeText (root </> targetModulePath target) moduleSource
      writeText (root </> targetPropertiesPath target) (renderProperties (targetName target) (artefact run))
      original <- readText (root </> cabal)
      case addExposedModule (targetName target) original of
        Left err -> pure (Left err)
        Right withModule -> do
          let updated = addTestSuite (targetPkg target) (targetName target) withModule
          if updated /= original then writeText (root </> cabal) updated else pure ()
          let touched = [targetModulePath target, targetPropertiesPath target] ++ [cabal | updated /= original]
          pure (Right (nub (touchedFiles run ++ touched), spans))

-- | The paths that exist but were not created by this run.
existingFiles :: FilePath -> Run -> [FilePath] -> IO [FilePath]
existingFiles root run paths = do
  existing <- mapM (\p -> (,) p <$> doesFileExist (root </> p)) paths
  pure [p | (p, True) <- existing, p `notElem` touchedFiles run]

writeText :: FilePath -> Text -> IO ()
writeText path value = do
  createDirectoryIfMissing True (takeDirectory path)
  B.writeFile path (TE.encodeUtf8 value)

readText :: FilePath -> IO Text
readText path = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') <$> B.readFile path

-- | Whether git reports uncommitted changes. 'False' outside a repository.
uncommittedChanges :: FilePath -> IO Bool
uncommittedChanges root = do
  result <- try (readCreateProcessWithExitCode (proc "git" ["status", "--porcelain"]) {cwd = Just root} "")
  pure $ case result :: Either IOException (ExitCode, String, String) of
    Right (ExitSuccess, out, _) -> not (null (lines out))
    _ -> False
