{-# LANGUAGE OverloadedStrings #-}

-- | Building and running a run's property suite with the project's cabal, and
-- reading the results.
module Verify
  ( GhcError (..),
    buildSuite,
    runSuite,
    ghcErrors,
    attributeErrors,
    failedProperties,
    summarise,
  )
where

import Data.Char (isDigit)
import Data.List (isSuffixOf, nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Render (Span)
import System.Exit (ExitCode (..))
import System.FilePath (normalise)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)
import Core (FunctionName)

-- | Builds the suite, and with it the library. 'Left' holds the build output.
buildSuite :: FilePath -> Text -> IO (Either Text ())
buildSuite root target = fmap (const ()) <$> cabal root ["build", T.unpack target]

-- | Runs the suite. 'Left' holds the output of a failed run.
runSuite :: FilePath -> Text -> IO (Either Text Text)
runSuite root target = cabal root ["run", "-v1", T.unpack target]

cabal :: FilePath -> [String] -> IO (Either Text Text)
cabal root args = do
  result <- timeout (15 * 60 * 1000000) (readCreateProcessWithExitCode (proc "cabal" args) {cwd = Just root} "")
  pure $ case result of
    Nothing -> Left ("cabal " <> T.pack (unwords args) <> " timed out after 15 minutes")
    Just (ExitSuccess, out, err) -> Right (T.pack (out <> err))
    Just (ExitFailure _, out, err) -> Left (T.pack (out <> err))

data GhcError = GhcError
  { errorFile :: FilePath,
    errorLine :: Int,
    errorText :: Text
  }
  deriving (Eq, Show)

-- | GHC errors in build output. Each starts with a @file:line:col: error@
-- header and runs to the next blank line.
ghcErrors :: Text -> [GhcError]
ghcErrors = go . T.lines
  where
    go [] = []
    go (l : rest) = case header l of
      Just (file, line) ->
        let (body, rest') = break (T.null . T.strip) rest
         in GhcError file line (T.unlines (l : body)) : go rest'
      Nothing -> go rest
    header l = case T.breakOn ": error" l of
      (location, suffix) | not (T.null suffix) -> parseLocation location
      _ -> Nothing

-- | @file:line:col@ or @file:(line,col)-(line,col)@.
parseLocation :: Text -> Maybe (FilePath, Int)
parseLocation location
  | ")" `T.isSuffixOf` location =
      let (file, range) = T.breakOn ":(" location
       in (,) (T.unpack file) <$> number (T.drop 2 range)
  | otherwise = case T.splitOn ":" location of
      parts@(_ : _ : _ : _) ->
        let file = T.intercalate ":" (take (length parts - 2) parts)
         in (,) (T.unpack file) <$> number (parts !! (length parts - 2))
      _ -> Nothing
  where
    number t = case T.takeWhile isDigit t of
      digits | not (T.null digits) -> Just (read (T.unpack digits))
      _ -> Nothing

-- | Assigns errors in the module to the functions whose spans contain them.
-- Everything else (types, imports, the property suite) is unattributed.
attributeErrors :: FilePath -> [(FunctionName, Span)] -> [GhcError] -> ([(FunctionName, Text)], [Text])
attributeErrors file spans errors = (attributed, [errorText e | e <- errors, owner e == Nothing])
  where
    attributed =
      [ (name, T.unlines [errorText e | e <- errors, owner e == Just name])
      | name <- nub (mapMaybe owner errors)
      ]
    owner e
      | normalise file `isSuffixOf` normalise (errorFile e) || normalise (errorFile e) `isSuffixOf` normalise file =
          case [name | (name, (from, to)) <- spans, errorLine e >= from, errorLine e <= to] of
            name : _ -> Just name
            [] -> Nothing
      | otherwise = Nothing

-- | Properties reported as failed (or given up on) in the suite's output.
failedProperties :: Text -> [Text]
failedProperties = go Nothing . T.lines
  where
    go _ [] = []
    go current (l : rest)
      | Just name <- T.stripPrefix "=== " l = go (Just (T.strip name)) rest
      | "*** " `T.isPrefixOf` l, Just name <- current = name : go Nothing rest
      | otherwise = go current rest

-- | The errors in build output, or its tail if none could be found, limited
-- to a size that fits in a prompt.
summarise :: Text -> Text
summarise output = T.take 6000 $ case ghcErrors output of
  [] -> T.unlines (lastN 40 (T.lines output))
  errors -> T.intercalate "\n" (map errorText errors)
  where
    lastN n xs = drop (length xs - n) xs
