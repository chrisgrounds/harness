{-# LANGUAGE OverloadedStrings #-}

-- | Writing a run's artefact into the user's project.
module Workspace
  ( Originals,
    newOriginals,
    original,
    rollback,
    Suite (..),
    suiteOf,
    suiteImports,
    plannedFiles,
    writeArtefact,
    existingFiles,
    readText,
    resolveErrorFile,
    uncommittedChanges,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM, forM_, unless, when)
import Core
import qualified Data.ByteString as B
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find, foldl', isPrefixOf, isSuffixOf, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Project
import Render (renderFile, renderProperties)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, isRelative, makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, takeFileName, (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import Verify (GhcError (..))

-- | Files as they were before the run, read the first time they are needed
-- and before the run writes them, so every rendering starts from them and
-- a run can be undone. For files that did not exist, the directories above
-- them that did not exist either.
data Originals = Originals (IORef (Map.Map FilePath (Maybe Text))) (IORef [FilePath])

newOriginals :: IO Originals
newOriginals = Originals <$> newIORef Map.empty <*> newIORef []

-- | A file's contents before the run, or 'Nothing' if it did not exist.
original :: FilePath -> Originals -> FilePath -> IO (Maybe Text)
original root (Originals ref dirs) path = do
  cached <- Map.lookup path <$> readIORef ref
  case cached of
    Just known -> pure known
    Nothing -> do
      exists <- doesFileExist (root </> path)
      contents <- if exists then Just <$> readText (root </> path) else pure Nothing
      missing <- if exists then pure [] else filterM (fmap not . doesDirectoryExist . (root </>)) (ancestors path)
      atomicModifyIORef' dirs (\ds -> (nub (ds ++ missing), ()))
      atomicModifyIORef' ref (\m -> (Map.insertWith (\_ old -> old) path contents m, ()))
      fromMaybe contents . Map.lookup path <$> readIORef ref
  where
    ancestors = takeWhile (/= ".") . drop 1 . iterate takeDirectory

-- | Puts every file the run changed back as it was: existing files get their
-- original contents, and files and directories the run created are
-- removed. Returns the files restored or removed.
rollback :: FilePath -> Originals -> Run -> IO [FilePath]
rollback root originals@(Originals _ dirs) run = do
  undone <- fmap catMaybes . forM (touchedFiles run) $ \path -> do
    before <- original root originals path
    case before of
      Just text -> writeIfChanged (root </> path) text >> pure (Just path)
      Nothing -> do
        exists <- doesFileExist (root </> path)
        when exists (removeFile (root </> path))
        pure (if exists then Just path else Nothing)
  created <- readIORef dirs
  -- Deepest first, and only those left empty.
  forM_ (sortOn (Down . length . splitDirectories) created) $ \dir -> do
    exists <- doesDirectoryExist (root </> dir)
    empty <- if exists then null <$> listDirectory (root </> dir) else pure False
    when empty (removeDirectory (root </> dir))
  pure undone

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

-- | The plan's modules the property suite can import.
suiteImports :: [Component] -> Suite -> Artefact -> [ModuleName]
suiteImports cs suite art = [planModule p | p <- modules art, visibleFrom cs (suiteComponent suite) (planComponent p)]

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
            -- Suites written for an earlier main module, which would still be built.
            let oldSuites = [p | p <- touchedFiles run, isPropertiesFile p, p /= suiteFile suite]
            mapM_ (removeIfPresent . (root </>)) oldSuites
            restored <- restore [p | p <- touchedFiles run, p `notElem` plannedFiles art, p `notElem` newFiles, p `notElem` oldSuites]
            _ <- original root originals (suiteFile suite)
            writeIfChanged (root </> suiteFile suite) (renderProperties (suiteImports cs suite art) art)
            cabalFiles <- registerModules suite oldSuites
            let changed = [path | (path, source, text) <- rendered, source /= Just text]
                kept = [p | p <- touchedFiles run, p `notElem` restored, p `notElem` oldSuites]
            pure (Right (nub (kept ++ changed ++ [suiteFile suite] ++ cabalFiles)))
  where
    art = artefact run
    -- Files an earlier design changed, back to how they were.
    restore paths = fmap catMaybes . mapM restoreOne $ paths
    restoreOne path = do
      before <- original root originals path
      case before of
        Just text | not (".cabal" `T.isSuffixOf` T.pack path) -> writeIfChanged (root </> path) text >> pure (Just path)
        _ -> pure Nothing
    registerModules suite oldSuites = do
      let newModules = [(c, planModule p) | p <- modules art, planIsNew p, Just c <- [find ((== planComponent p) . componentName) cs]]
          main = maybe (ModuleName "") planModule (mainModule art)
          cabals = nub (cabalFile (suiteComponent suite) : [p | p <- touchedFiles run, takeExtension p == ".cabal"])
          edits =
            [(cabal, removeTestSuite (takeFileName old)) | old <- oldSuites, cabal <- cabals]
              ++ [(cabalFile c, \text -> either (const text) id (addModule c m text)) | (c, m) <- newModules]
              ++ [ ( cabalFile (suiteComponent suite),
                     upsertTestSuite (suiteComponent suite) main [m | (c, m) <- newModules, c == suiteComponent suite]
                   )
                 ]
      fmap catMaybes . mapM (editCabal edits) . nub $ map fst edits
    editCabal edits file = do
      -- Kept so the run can be undone.
      _ <- original root originals file
      before <- readText (root </> file)
      let after = foldl' (\text (_, edit) -> edit text) before [e | e@(f, _) <- edits, f == file]
      unless (after == before) $ writeText (root </> file) after
      pure (if after /= before || file `elem` touchedFiles run then Just file else Nothing)

-- | A property suite holey writes, @Holey<Module>Properties.hs@.
isPropertiesFile :: FilePath -> Bool
isPropertiesFile path = "Holey" `isPrefixOf` name && "Properties.hs" `isSuffixOf` name
  where
    name = takeFileName path

removeIfPresent :: FilePath -> IO ()
removeIfPresent path = do
  exists <- doesFileExist path
  when exists (removeFile path)

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
