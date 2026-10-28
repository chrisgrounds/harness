{-# LANGUAGE OverloadedStrings #-}

-- | Writing a run's artefact into the user's project.
module Workspace
  ( Originals,
    newOriginals,
    original,
    Suite (..),
    suiteOf,
    plannedFiles,
    writeArtefact,
    existingFiles,
    readText,
    resolveErrorFile,
    uncommittedChanges,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM_, unless)
import Core
import qualified Data.ByteString as B
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find, foldl', nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Project
import Render (renderFile, renderProperties)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, isRelative, makeRelative, normalise, takeDirectory, (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Verify (GhcError (..))

-- | Files as they were before the run, read the first time they are needed
-- and before the run writes them, so every rendering starts from them.
newtype Originals = Originals (IORef (Map.Map FilePath (Maybe Text)))

newOriginals :: IO Originals
newOriginals = Originals <$> newIORef Map.empty

-- | A file's contents before the run, or 'Nothing' if it did not exist.
original :: FilePath -> Originals -> FilePath -> IO (Maybe Text)
original root (Originals ref) path = do
  cached <- Map.lookup path <$> readIORef ref
  case cached of
    Just known -> pure known
    Nothing -> do
      exists <- doesFileExist (root </> path)
      contents <- if exists then Just <$> readText (root </> path) else pure Nothing
      atomicModifyIORef' ref (\m -> (Map.insertWith (\_ old -> old) path contents m, ()))
      maybe contents id . Map.lookup path <$> readIORef ref

-- | The run's property suite: the component it is built against, its file
-- and its cabal target.
data Suite = Suite
  { suiteComponent :: Component,
    suiteFile :: FilePath,
    suiteCabalTarget :: Text
  }

suiteOf :: [Component] -> Artefact -> Maybe Suite
suiteOf cs art = do
  plan <- mainModule art
  c <- find ((== planComponent plan) . componentName) cs
  pure (Suite c (propertiesPath c (planModule plan)) (suiteTarget c (planModule plan)))

-- | The files the run writes code into: its modules, and the files of the
-- functions it repairs.
plannedFiles :: Artefact -> [FilePath]
plannedFiles art = nub (map planPath (modules art) ++ map (refPath . fnRef) (functions art))

-- | Writes every file in the plan from its original, restores files the
-- plan no longer changes, and registers new modules and the property suite
-- in the cabal files. Refuses to overwrite files the run did not create.
-- Returns every file the run has changed.
writeArtefact :: FilePath -> [Component] -> Originals -> Run -> IO (Either Text [FilePath])
writeArtefact root cs originals run = case suiteOf cs art of
  Nothing -> pure (Left "No module was chosen.")
  Just suite -> do
    let newFiles = [planPath p | p <- modules art, planIsNew p] ++ [suiteFile suite]
    clashes <- existingFiles root run newFiles
    if not (null clashes)
      then pure (Left ("Refusing to overwrite existing files: " <> T.intercalate ", " (map T.pack clashes)))
      else do
        sources <- mapM (\path -> (,) path <$> original root originals path) (plannedFiles art)
        case traverse (\(path, source) -> (,,) path source <$> renderFile art path source) sources of
          Left err -> pure (Left err)
          Right rendered -> do
            forM_ rendered $ \(path, _, text) -> writeIfChanged (root </> path) text
            restored <- restore [p | p <- touchedFiles run, p `notElem` plannedFiles art, p `notElem` newFiles]
            writeIfChanged (root </> suiteFile suite) (renderProperties art)
            cabalFiles <- registerModules suite
            let changed = [path | (path, source, text) <- rendered, source /= Just text]
            pure (Right (nub ([p | p <- touchedFiles run, p `notElem` restored] ++ changed ++ [suiteFile suite] ++ cabalFiles)))
  where
    art = artefact run
    -- Files an earlier design changed, back to how they were.
    restore paths = fmap catMaybes . mapM restoreOne $ paths
    restoreOne path = do
      before <- original root originals path
      case before of
        Just text | not (".cabal" `T.isSuffixOf` T.pack path) -> writeIfChanged (root </> path) text >> pure (Just path)
        _ -> pure Nothing
    registerModules suite = do
      let newModules = [(c, planModule p) | p <- modules art, planIsNew p, Just c <- [find ((== planComponent p) . componentName) cs]]
          main = maybe (ModuleName "") planModule (mainModule art)
          edits =
            [(cabalFile c, \text -> either (const text) id (addModule c m text)) | (c, m) <- newModules]
              ++ [ ( cabalFile (suiteComponent suite),
                     upsertTestSuite (suiteComponent suite) main [m | (c, m) <- newModules, c == suiteComponent suite]
                   )
                 ]
      fmap catMaybes . mapM (editCabal edits) . nub $ map fst edits
    editCabal edits file = do
      before <- readText (root </> file)
      let after = foldl' (\text (_, edit) -> edit text) before [e | e@(f, _) <- edits, f == file]
      unless (after == before) $ writeText (root </> file) after
      pure (if after /= before || file `elem` touchedFiles run then Just file else Nothing)

-- | The paths that exist but were not created by this run.
existingFiles :: FilePath -> Run -> [FilePath] -> IO [FilePath]
existingFiles root run paths = do
  existing <- mapM (\p -> (,) p <$> doesFileExist (root </> p)) paths
  pure [p | (p, True) <- existing, p `notElem` touchedFiles run]

-- | The file in the project an error is in, relative to the project root.
-- GHC reports paths relative to the package being built, so the package
-- cabal was building is tried first.
resolveErrorFile :: FilePath -> ProjectInfo -> GhcError -> IO (Maybe FilePath)
resolveErrorFile root info e
  | isAbsolute file = pure (inProject (makeRelative root file))
  | otherwise = do
      let building = [dir | Just name <- [errorPackage e], Just dir <- [lookup name (packages info)]]
          candidates = nub (map normalise ([d </> file | d <- building] ++ [file] ++ [d </> file | (_, d) <- packages info]))
      found <- filterM (doesFileExist . (root </>)) candidates
      pure (case found of
        p : _ -> inProject p
        [] -> Nothing)
  where
    file = errorFile e
    inProject p = if isRelative p && take 2 p /= ".." then Just (normalise p) else Nothing

writeIfChanged :: FilePath -> Text -> IO ()
writeIfChanged path value = do
  exists <- doesFileExist path
  current <- if exists then Just <$> readText path else pure Nothing
  unless (current == Just value) $ writeText path value

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
