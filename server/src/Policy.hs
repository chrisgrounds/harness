{-# LANGUAGE OverloadedStrings #-}

-- | What a run does after a verification fails.
module Policy (Outcome (..), Decision (..), maxIterations, decide, describeOutcome) where

import Core (FunctionName (..))
import Data.Text (Text)
import qualified Data.Text as T

-- | How a verification step failed.
data Outcome
  = -- | The skeleton (types, signatures, properties) did not build.
    SkeletonFailed Text
  | -- | These functions' agents did not submit an implementation.
    ImplementationMissing [FunctionName]
  | -- | Errors in each function's lines, and errors that could not be attributed.
    TypeErrors [(FunctionName, Text)] [Text]
  | -- | The failed properties, the functions they mention, and their output.
    PropertiesFailed [Text] [FunctionName] Text
  deriving (Eq, Show)

data Decision
  = -- | Back to step 1, with feedback for the design conversation.
    Redesign Text
  | -- | Back to step 4 for these functions, each with optional feedback.
    Reimplement [(FunctionName, Maybe Text)]
  | GiveUp Text
  deriving (Eq, Show)

-- | Iterations a run may use, including the first.
maxIterations :: Int
maxIterations = 10

-- | Decides how to continue on the given iteration, knowing every function in
-- the module and the properties that failed on the previous iteration.
decide :: Int -> [FunctionName] -> [Text] -> Outcome -> Decision
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

list :: [FunctionName] -> Text
list = T.intercalate ", " . map functionNameText
