{-# LANGUAGE OverloadedStrings #-}

-- | What a run does after a verification fails.
module Policy (Outcome (..), Decision (..), maxIterations, decide, describeOutcome, redesignReason) where

import Core (FunctionName (..), FunctionRef (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Verify (firstError)

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

-- | A one-line description for the run's history.
describeOutcome :: Outcome -> Text
describeOutcome (SkeletonFailed _) = "the skeleton did not type check"
describeOutcome (ImplementationMissing names) = "no implementation was submitted for " <> list names
describeOutcome (TypeErrors attributed _)
  | null attributed = "the module did not type check"
  | otherwise = "type errors in " <> list (map fst attributed)
describeOutcome (PropertiesFailed failed _ _) = "properties failed: " <> T.intercalate ", " failed

-- | Why an outcome sends the run back to design, in one sentence.
redesignReason :: Outcome -> Text
redesignReason (SkeletonFailed errors) =
  case filter (not . T.null) (map T.strip (T.lines errors)) of
    first : _ | "The signatures of these existing functions" `T.isPrefixOf` first -> first
    lines' -> fromMaybe (maybe "the build failed without a GHC error." id (safeHead lines')) (firstError errors)
  where
    safeHead (x : _) = Just x
    safeHead [] = Nothing
redesignReason (PropertiesFailed {}) = "they still fail after the functions they mention were reimplemented, so the properties or the design may be wrong."
redesignReason _ = "it could not be fixed by reimplementing functions."

list :: [FunctionRef] -> Text
list = T.intercalate ", " . map (functionNameText . refName)
