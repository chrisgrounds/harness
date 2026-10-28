{-# LANGUAGE OverloadedStrings #-}

-- | What a run does after a verification fails.
module Policy (Outcome (..), Decision (..), maxIterations, decide, failedChecks, describeOutcome, describeWork, redesignReason) where

import Core (FunctionName (..), FunctionRef (..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Verify (firstProblem)

-- | How a verification step failed.
data Outcome
  = -- | The skeleton (types, signatures, properties) did not build.
    SkeletonFailed Text
  | -- | These functions' agents did not submit an implementation.
    ImplementationMissing [FunctionRef]
  | -- | Errors in each function's lines, and errors that could not be attributed.
    TypeErrors [(FunctionRef, Text)] [Text]
  | -- | The failed properties, the functions they mention, and their output.
    PropertiesFailed [Text] [FunctionRef] Text
  deriving (Eq, Show)

data Decision
  = -- | Back to step 1, with feedback for the design conversation.
    Redesign Text
  | -- | Back to step 4 for these functions, each with optional feedback.
    Reimplement [(FunctionRef, Maybe Text)]
  | GiveUp Text
  deriving (Eq, Show)

-- | Iterations a run may use, including the first.
maxIterations :: Int
maxIterations = 10

-- | Decides how to continue on the given iteration, knowing every function the
-- run writes and the properties that failed on the previous iteration.
decide :: Int -> [FunctionRef] -> [Text] -> Outcome -> Decision
decide iterationNumber allFunctions previouslyFailed outcome
  | iterationNumber >= maxIterations =
      GiveUp ("Gave up after " <> T.pack (show maxIterations) <> " iterations: " <> describeOutcome outcome)
  | otherwise = case outcome of
      SkeletonFailed errors -> Redesign errors
      ImplementationMissing missing -> Reimplement [(name, Nothing) | name <- missing]
      TypeErrors attributed unattributed
        | null attributed && any (`elem` previouslyFailed) (failedChecks outcome) ->
            Redesign
              ( "These build errors are outside the run's functions and came back after every function was reimplemented, so the design has to change:\n"
                  <> T.unlines unattributed
              )
        | null attributed -> Reimplement [(name, Just (T.unlines unattributed)) | name <- allFunctions]
        | otherwise -> Reimplement [(name, Just (T.unlines (errors : unattributed))) | (name, errors) <- attributed]
      PropertiesFailed failed suspects report
        | any (`elem` previouslyFailed) failed ->
            Redesign
              ( "These properties still fail after their functions were reimplemented, so the properties or the design may be wrong: "
                  <> T.intercalate ", " failed
                  <> "\n"
                  <> report
              )
        | null suspects -> Reimplement [(name, Just report) | name <- allFunctions]
        | otherwise -> Reimplement [(name, Just report) | name <- suspects]

-- | What an outcome failed, to tell whether the next iteration fails the same
-- way: the failed properties, or the first build error outside every
-- function, without where it is.
failedChecks :: Outcome -> [Text]
failedChecks (PropertiesFailed failed _ _) = failed
failedChecks (TypeErrors [] unattributed) = ["build: " <> maybe "" withoutLocation (firstProblem (T.unlines unattributed))]
  where
    withoutLocation problem = case T.breakOnEnd " (" problem of
      (before, location) | not (T.null before), ")" `T.isSuffixOf` location -> T.dropEnd 2 before
      _ -> problem
failedChecks _ = []

-- | A one-line description for the run's history.
describeOutcome :: Outcome -> Text
describeOutcome (SkeletonFailed _) = "the skeleton did not type check"
describeOutcome (ImplementationMissing names) = "no implementation was submitted for " <> list names
describeOutcome (TypeErrors attributed unattributed)
  | null attributed = "the build failed outside the run's functions: " <> fromMaybe "no error was reported" (firstProblem (T.unlines unattributed))
  | otherwise = "type errors in " <> list (map fst attributed)
describeOutcome (PropertiesFailed failed _ _) = "properties failed: " <> T.intercalate ", " failed

-- | Why an outcome sends the run back to design, in one sentence.
redesignReason :: Outcome -> Text
redesignReason (SkeletonFailed errors) =
  case filter (not . T.null) (map T.strip (T.lines errors)) of
    first : _ | "The signatures of these existing functions" `T.isPrefixOf` first -> first
    lines' -> fromMaybe (fromMaybe "the build failed without a GHC error." (listToMaybe lines')) (firstProblem errors)
redesignReason (PropertiesFailed {}) = "they still fail after the functions they mention were reimplemented, so the properties or the design may be wrong."
redesignReason (TypeErrors {}) = "it came back after every function was reimplemented, so the design has to change."
redesignReason _ = "it could not be fixed by reimplementing functions."

-- | The functions an iteration reimplements, out of all the run's: all of
-- them, a short list, or the first few and how many more.
describeWork :: [FunctionRef] -> [FunctionRef] -> Text
describeWork everything work
  | length work > 3 && all (`elem` work) everything = "all " <> T.pack (show (length work)) <> " functions"
  | length work > 6 = list (take 5 work) <> " and " <> T.pack (show (length work - 5)) <> " more"
  | otherwise = list work

list :: [FunctionRef] -> Text
list = T.intercalate ", " . map (functionNameText . refName)
