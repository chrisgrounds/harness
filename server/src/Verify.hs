{-# LANGUAGE OverloadedStrings #-}

-- | Building and running a run's property suite with the project's cabal, and
-- reading the results.
module Verify
  ( GhcError (..),
    Blame (..),
    buildAll,
    buildTarget,
    runSuite,
    ghcErrors,
    blame,
    firstError,
    firstProblem,
    failedProperties,
    propertyReport,
    mentions,
    summarise,
    brief,
  )
where

import Core (FunctionName (..), FunctionRef (..), FunctionSpec (..))
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Decls
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

-- | Builds every package in the project with its test suites, so code that
-- uses what the run changed is type checked too. One job at a time keeps
-- each package's errors after its "Building" line. 'Left' holds the output.
buildAll :: FilePath -> IO (Either Text ())
buildAll root = fmap (const ()) <$> cabal root ["build", "all", "--enable-tests", "-j1"]

buildTarget :: FilePath -> Text -> IO (Either Text ())
buildTarget root target = fmap (const ()) <$> cabal root ["build", "--enable-tests", T.unpack target]

-- | Runs the run's property suite. 'Left' holds the output of a failed run.
runSuite :: FilePath -> Text -> IO (Either Text Text)
runSuite root target = cabal root ["run", "--enable-tests", "-v1", T.unpack target]

cabal :: FilePath -> [String] -> IO (Either Text Text)
cabal root args = do
  result <- timeout (15 * 60 * 1000000) (readCreateProcessWithExitCode (proc "cabal" args) {cwd = Just root} "")
  pure $ case result of
    Nothing -> Left ("cabal " <> T.pack (unwords args) <> " timed out after 15 minutes")
    Just (ExitSuccess, out, err) -> Right (T.pack (out <> err))
    Just (ExitFailure _, out, err) -> Left (T.pack (out <> err))

data GhcError = GhcError
  { -- | As GHC reports it: usually relative to the package's directory.
    errorFile :: FilePath,
    errorLine :: Int,
    errorText :: Text,
    -- | The package cabal was building when the error was reported.
    errorPackage :: Maybe Text
  }
  deriving (Eq, Show)

-- | GHC errors in build output. Each starts with a @file:line:col: error@
-- header and runs to the next blank line.
ghcErrors :: Text -> [GhcError]
ghcErrors = go Nothing . T.lines
  where
    go _ [] = []
    go package (l : rest) = case header l of
      Just (file, line) ->
        let (body, rest') = break (T.null . T.strip) rest
         in GhcError file line (T.unlines (l : body)) package : go package rest'
      Nothing -> go (maybe package Just (building l)) rest
    header l = case T.breakOn ": error" l of
      (location, suffix) | not (T.null suffix) -> parseLocation location
      _ -> Nothing

-- | The package in a line such as @Building library for core-0.1.0.0..@.
building :: Text -> Maybe Text
building l
  | any (`T.isPrefixOf` l) ["Building ", "Preprocessing "], (_, rest) <- T.breakOnEnd " for " l, not (T.null rest) =
      case T.words (T.dropWhileEnd (== '.') rest) of
        packageId : _ -> Just (withoutVersion packageId)
        [] -> Nothing
  | otherwise = Nothing
  where
    withoutVersion packageId = case T.breakOnEnd "-" packageId of
      (name, version) | not (T.null name), T.all (\c -> isDigit c || c == '.') version -> T.dropEnd 1 name
      _ -> packageId

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

-- | Where an error is.
data Blame
  = -- | In a function the run designed or is repairing, or in an import line
    -- one of them added.
    InPlanned FunctionRef
  | -- | In another function or instance in the project, which the design broke.
    InOther FunctionRef
  | -- | In a type, an instance without methods, the property suite, or
    -- outside the project.
    Unblamed
  deriving (Eq, Show)

-- | Blames an error on the line of a file, given the run's functions and the
-- file as written. Functions and instances with methods can be repaired, so
-- an instance is blamed like a function, by its @instance <head>@ key.
blame :: [FunctionSpec] -> FilePath -> SourceFile -> Int -> Blame
blame fns path source line = case declAt line source of
  Just d
    | repairable d,
      Just key <- declKey d ->
        let ref = FunctionRef path (FunctionName key)
         in if ref `elem` map fnRef fns then InPlanned ref else InOther ref
  Just _ -> Unblamed
  Nothing -> case importAt line source of
    Just imported
      | f : _ <- [f | f <- fns, refPath (fnRef f) == path, imported `elem` map (T.unwords . T.words) (fnImports f)] -> InPlanned (fnRef f)
    _ -> Unblamed

repairable :: Decl -> Bool
repairable d = declKind d == ValueDecl || (declKind d == InstanceDecl && isJust (instanceHeadLines d))

-- | The first problem in build output as one line: the first GHC error, or
-- else cabal's error and the line after it.
firstProblem :: Text -> Maybe Text
firstProblem output = firstError output <|> cabalError
  where
    cabalError = case dropWhile (not . ("Error:" `T.isPrefixOf`)) (map T.strip (T.lines output)) of
      problem : rest ->
        let detail = take 1 (filter (not . T.null) rest)
         in Just (clip (T.unwords (problem : detail)))
      [] -> Nothing
    clip t = if T.length t > 200 then T.take 197 t <> "..." else t

-- | The first GHC error in output as one line: its message and where it is.
firstError :: Text -> Maybe Text
firstError output = case ghcErrors output of
  e : _ ->
    let message = takeWhile (not . aside) . dropWhile T.null . map T.strip . drop 1 . T.lines $ errorText e
        firstPoint = case message of
          first : rest -> first : takeWhile (not . ("•" `T.isPrefixOf`)) rest
          [] -> []
        sentence = T.unwords (map (T.strip . dropBullet) firstPoint)
        clipped = if T.length sentence > 160 then T.take 157 sentence <> "..." else sentence
     in Just (clipped <> " (" <> T.pack (errorFile e) <> ":" <> T.pack (show (errorLine e)) <> ")")
  [] -> Nothing
  where
    -- The source excerpt, @12 | code@ and its gutter, and GHC's suggestions.
    aside l =
      "|" `T.isPrefixOf` l
        || "Suggested fix" `T.isPrefixOf` l
        || let (number, rest) = T.breakOn " |" l in not (T.null rest) && not (T.null number) && T.all isDigit number
    dropBullet l = maybe l T.strip (T.stripPrefix "•" l)

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

-- | The output of the failed properties, with their counterexamples.
propertyReport :: Text -> Text
propertyReport output = case failedProperties output of
  [] -> summarise output
  names -> T.take 6000 (T.intercalate "\n" [section | (name, section) <- sections, name `elem` names])
  where
    sections = go (T.lines output)
    go [] = []
    go (l : rest) = case T.stripPrefix "=== " l of
      Just name ->
        let (body, rest') = break ("=== " `T.isPrefixOf`) rest
         in (T.strip name, T.unlines (l : body)) : go rest'
      Nothing -> go rest

-- | Whether the source uses the function's name as an identifier.
mentions :: FunctionName -> Text -> Bool
mentions (FunctionName name) source = name `elem` T.split (not . isIdentChar) source

-- | The first lines of a report, for showing under a failed step.
brief :: Text -> Text
brief report
  | length ls > 8 = T.unlines (take 8 ls) <> "…"
  | otherwise = T.stripEnd report
  where
    ls = T.lines (T.strip report)
