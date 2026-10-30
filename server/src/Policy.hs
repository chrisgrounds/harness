{-# LANGUAGE OverloadedStrings #-}

module Policy (Outcome (..), Decision (..), maxIterations, decide, failedChecks, describeOutcome, describeWork, redesignReason) where

import Core (FunctionName (..), FunctionRef (..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Verify (firstProblem)

data Outcome
  = SkeletonFailed Text
  | ImplementationMissing [FunctionRef]
  | TypeErrors [(FunctionRef, Text)] [Text]
  | PropertiesFailed [Text] [FunctionRef] Text
  | RequirementUnmet Text
  deriving (Eq, Show)

data Decision
  = Redesign Text
  | Reimplement [(FunctionRef, Maybe Text)]
  | GiveUp Text
  deriving (Eq, Show)

maxIterations :: Int
maxIterations = 10

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
        | any (`elem` previouslyFailed) (failedChecks outcome) ->
            Redesign
              ( "These errors came back in the same functions after they were reimplemented, so they are probably caused by the design or by code elsewhere:\n"
                  <> T.unlines (map snd attributed ++ unattributed)
              )
        | otherwise -> Reimplement [(name, Just (T.unlines (errors : unattributed))) | (name, errors) <- attributed]
      RequirementUnmet reason -> Redesign ("The review found that the code does not meet the requirement: " <> reason)
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

failedChecks :: Outcome -> [Text]
failedChecks (PropertiesFailed failed _ _) = failed
failedChecks (TypeErrors [] unattributed) = ["build: " <> problem (T.unlines unattributed)]
failedChecks (TypeErrors attributed _) = [functionNameText (refName ref) <> ": " <> problem errors | (ref, errors) <- attributed]
failedChecks _ = []

problem :: Text -> Text
problem = maybe "" withoutLocation . firstProblem
  where
    withoutLocation text = case T.breakOnEnd " (" text of
      (before, location) | not (T.null before), ")" `T.isSuffixOf` location -> T.dropEnd 2 before
      _ -> text

describeOutcome :: Outcome -> Text
describeOutcome (SkeletonFailed _) = "the skeleton did not type check"
describeOutcome (ImplementationMissing names) = "no implementation was submitted for " <> list names
describeOutcome (TypeErrors attributed unattributed)
  | null attributed = "the build failed outside the run's functions: " <> fromMaybe "no error was reported" (firstProblem (T.unlines unattributed))
  | otherwise = "type errors in " <> list (map fst attributed)
describeOutcome (PropertiesFailed failed _ _) = "properties failed: " <> T.intercalate ", " failed
describeOutcome (RequirementUnmet _) = "the review found the requirement not met"

redesignReason :: Outcome -> Text
redesignReason (SkeletonFailed errors) =
  case filter (not . T.null) (map T.strip (T.lines errors)) of
    first : _ | "The signatures of these existing functions" `T.isPrefixOf` first -> first
    lines' -> fromMaybe (fromMaybe "the build failed without a GHC error." (listToMaybe lines')) (firstProblem errors)
redesignReason (PropertiesFailed {}) = "they still fail after the functions they mention were reimplemented, so the properties or the design may be wrong."
redesignReason (RequirementUnmet reason) = reason
redesignReason (TypeErrors [] _) = "it came back after every function was reimplemented, so the design has to change."
redesignReason (TypeErrors {}) = "they came back in the same functions after those were reimplemented, so the cause is probably elsewhere."
redesignReason _ = "it could not be fixed by reimplementing functions."

describeWork :: [FunctionRef] -> [FunctionRef] -> Text
describeWork everything work
  | length work > 3 && all (`elem` work) everything = "all " <> T.pack (show (length work)) <> " functions"
  | length work > 6 = list (take 5 work) <> " and " <> T.pack (show (length work - 5)) <> " more"
  | otherwise = list work

list :: [FunctionRef] -> Text
list = T.intercalate ", " . map (functionNameText . refName)
