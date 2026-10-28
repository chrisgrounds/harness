{-# LANGUAGE OverloadedStrings #-}

module Tools (loadToolDefinitions, readOnlyTool) where

import Control.Exception (IOException, try)
import Core
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Paths_server (getDataFileName)
import qualified System.Directory as Dir
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)

-- | Runs a tool that only reads the project at @root@, against which paths
-- resolve, or 'Nothing' for tools that change the run and are handled by the
-- orchestrator.
readOnlyTool :: FilePath -> ToolName -> Text -> Maybe (IO Text)
readOnlyTool root ReadFile args = Just $ withArgument "path" args $ \path ->
  reportIOError ("Could not read " <> path) (TIO.readFile (root </> T.unpack path))
readOnlyTool root Discover args = Just $ withArgument "path" args $ \path ->
  reportIOError ("Could not list " <> path) (T.unlines . map T.pack <$> Dir.listDirectory (root </> T.unpack path))
readOnlyTool root Search args = Just $ withArgument "pattern" args (search root)
readOnlyTool _ _ _ = Nothing

search :: FilePath -> Text -> IO Text
search root pattern = do
  (code, out, err) <-
    readCreateProcessWithExitCode
      (proc "grep" ["-rnIF", "--include=*.hs", "--include=*.cabal", "--exclude-dir=dist-newstyle", "--exclude-dir=.git", "--", T.unpack pattern, "."]) {cwd = Just root}
      ""
  pure $ case code of
    ExitSuccess -> truncateLines 200 (T.pack out)
    ExitFailure 1 -> "No matches."
    ExitFailure _ -> "Search failed: " <> T.pack err

truncateLines :: Int -> Text -> Text
truncateLines n value
  | length ls > n = T.unlines (take n ls) <> "... " <> T.pack (show (length ls - n)) <> " more lines"
  | otherwise = value
  where
    ls = T.lines value

withArgument :: Text -> Text -> (Text -> IO Text) -> IO Text
withArgument key args run =
  case eitherDecodeStrict (TE.encodeUtf8 args) >>= parseEither (withObject "arguments" (.: Key.fromText key)) of
    Left err -> pure ("Invalid arguments: " <> T.pack err)
    Right value -> run value

reportIOError :: Text -> IO Text -> IO Text
reportIOError context action = do
  result <- try action
  pure $ case result of
    Left err -> context <> ": " <> T.pack (show (err :: IOException))
    Right value -> value

-- | Every tool's definition, from its packaged schema, @tools/read_file.json@
-- for 'ReadFile' and so on.
loadToolDefinitions :: IO (ToolName -> ToolDefinition)
loadToolDefinitions = do
  loaded <- Map.fromList <$> mapM (\name -> (,) name <$> load name) [minBound .. maxBound]
  -- Every tool was loaded, so the lookup cannot fail.
  pure (loaded Map.!)
  where
    load name = do
      let path = "tools" </> camelTo2 '_' (show name) <.> "json"
      contents <- getDataFileName path >>= TIO.readFile
      either (\err -> fail ("Failed to parse the tool definition " <> path <> ": " <> err)) pure (eitherDecodeStrict (TE.encodeUtf8 contents))
