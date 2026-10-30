{-# LANGUAGE OverloadedStrings #-}

module Verify
  ( GhcError (..),
    Blame (..),
    buildAll,
    failOnFatalWarnings,
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

buildAll :: FilePath -> IO (Either Text Text)
buildAll root = failOnFatalWarnings <$> cabal root (["build", "all"] ++ verifyFlags)

-- | Verification only type checks and runs the properties, so it skips
-- optimisation and builds into its own directory, leaving the project's own
-- builds untouched. Packages build one at a time because cabal interleaves
-- concurrent packages' output character by character, which 'ghcErrors'
-- cannot read; GHC's -j compiles a package's modules in parallel and keeps
-- each message whole. Every command shares these flags, or cabal would
-- rebuild between them.
verifyFlags :: [String]
verifyFlags = ["--enable-tests", "--disable-optimization", "--builddir=dist-newstyle/holey-build", "-j1", "--ghc-options=-j"]

failOnFatalWarnings :: Either Text Text -> Either Text Text
failOnFatalWarnings (Right output) | any fatalWarning (T.lines output) = Left output
failOnFatalWarnings built = built

fatalWarning :: Text -> Bool
fatalWarning l = ": warning:" `T.isInfixOf` l && "[-Wmissing-fields]" `T.isInfixOf` l

buildTarget :: FilePath -> Text -> IO (Either Text ())
buildTarget root target = fmap (const ()) <$> cabal root (["build"] ++ verifyFlags ++ [T.unpack target])

runSuite :: FilePath -> Text -> IO (Either Text Text)
runSuite root target = cabal root (["run", "-v1"] ++ verifyFlags ++ [T.unpack target])

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
    errorText :: Text,
    errorPackage :: Maybe Text
  }
  deriving (Eq, Show)

ghcErrors :: Text -> [GhcError]
ghcErrors = go Nothing . T.lines
  where
    go _ [] = []
    go package (l : rest) = case header l of
      Just (file, line) ->
        let (body, rest') = break (T.null . T.strip) rest
         in GhcError file line (T.unlines (l : body)) package : go package rest'
      Nothing -> go (maybe package Just (building l)) rest
    header l = case T.breakOn (if fatalWarning l then ": warning" else ": error") l of
      (location, suffix)
        | T.null suffix -> Nothing
        | T.strip location == noLocation -> Just (T.unpack noLocation, 0)
        | otherwise -> parseLocation location
    noLocation = "<no location info>"

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

data Blame
  = InPlanned FunctionRef
  | InOther FunctionRef
  | Unblamed
  deriving (Eq, Show)

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

firstProblem :: Text -> Maybe Text
firstProblem output = firstError output <|> cabalError
  where
    cabalError = case dropWhile (not . ("Error:" `T.isPrefixOf`)) (map T.strip (T.lines output)) of
      problem : rest ->
        let detail = take 1 (filter (not . T.null) rest)
         in Just (clip (T.unwords (problem : detail)))
      [] -> Nothing
    clip t = if T.length t > 200 then T.take 197 t <> "..." else t

firstError :: Text -> Maybe Text
firstError output = case ghcErrors output of
  e : _ ->
    let message = takeWhile (not . aside) . dropWhile T.null . map T.strip . drop 1 . T.lines $ errorText e
        firstPoint = case message of
          first : rest -> first : takeWhile (not . ("•" `T.isPrefixOf`)) rest
          [] -> []
        sentence = T.unwords (map (T.strip . dropBullet) firstPoint)
        clipped = if T.length sentence > 160 then T.take 157 sentence <> "..." else sentence
     in Just (clipped <> if errorLine e == 0 then "" else " (" <> T.pack (errorFile e) <> ":" <> T.pack (show (errorLine e)) <> ")")
  [] -> Nothing
  where
    aside l =
      "|" `T.isPrefixOf` l
        || "Suggested fix" `T.isPrefixOf` l
        || let (number, rest) = T.breakOn " |" l in not (T.null rest) && not (T.null number) && T.all isDigit number
    dropBullet l = maybe l T.strip (T.stripPrefix "•" l)

failedProperties :: Text -> [Text]
failedProperties = go Nothing . T.lines
  where
    go _ [] = []
    go current (l : rest)
      | Just name <- T.stripPrefix "=== " l = go (Just (T.strip name)) rest
      | "*** " `T.isPrefixOf` l, Just name <- current = name : go Nothing rest
      | otherwise = go current rest

summarise :: Text -> Text
summarise output = T.take 6000 $ case ghcErrors output of
  [] -> case dropWhile (not . ("Error:" `T.isPrefixOf`) . T.strip) ls of
    [] -> T.unlines (drop (length ls - 40) ls)
    cabalError -> T.unlines cabalError
  errors -> T.intercalate "\n" (map errorText errors)
  where
    ls = T.lines output

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

mentions :: FunctionName -> Text -> Bool
mentions (FunctionName name) source = name `elem` T.split (not . isIdentChar) source

brief :: Text -> Text
brief report
  | length ls > 8 = T.unlines (take 8 ls) <> "…"
  | otherwise = T.stripEnd report
  where
    ls = T.lines (T.strip report)
