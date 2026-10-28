{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Client.Commands
import UI
import Test.QuickCheck
import qualified Test.QuickCheck as HoleyQuickCheck
import qualified System.Exit as HoleyExit
import qualified Data.Text as T
import Data.List (nub, sort)
import Core (ToolName, toolNameText, Requirement (..))

-- | A non-empty text that is never '/quit' or '/tools': it consists of
-- lowercase letters, so it contains no slashes.
nonCommandText :: Gen T.Text
nonCommandText = T.pack <$> listOf1 (elements ['a' .. 'z'])

prop_parseQuit :: Bool
prop_parseQuit = parseCommand "/quit" == Quit

prop_parseTools :: Bool
prop_parseTools = parseCommand "/tools" == ListTools

prop_parseNonCommandIsRequirement :: Property
prop_parseNonCommandIsRequirement =
  forAll nonCommandText $ \t ->
    parseCommand t == RunRequirement (Requirement t)

prop_toolNamesAllNonEmpty :: Bool
prop_toolNamesAllNonEmpty = all (not . T.null) allToolNames

prop_toolNamesUnique :: Bool
prop_toolNamesUnique = length allToolNames == length (nub allToolNames)

prop_toolNamesMatchEnumeration :: Bool
prop_toolNamesMatchEnumeration =
  sort allToolNames == sort (map toolNameText ([minBound .. maxBound] :: [ToolName]))

main :: IO ()
main = do
  results <-
    sequence
      [ putStrLn "=== prop_parseQuit" >> HoleyQuickCheck.quickCheckResult prop_parseQuit
      , putStrLn "=== prop_parseTools" >> HoleyQuickCheck.quickCheckResult prop_parseTools
      , putStrLn "=== prop_parseNonCommandIsRequirement" >> HoleyQuickCheck.quickCheckResult prop_parseNonCommandIsRequirement
      , putStrLn "=== prop_toolNamesAllNonEmpty" >> HoleyQuickCheck.quickCheckResult prop_toolNamesAllNonEmpty
      , putStrLn "=== prop_toolNamesUnique" >> HoleyQuickCheck.quickCheckResult prop_toolNamesUnique
      , putStrLn "=== prop_toolNamesMatchEnumeration" >> HoleyQuickCheck.quickCheckResult prop_toolNamesMatchEnumeration
      ]
  if all HoleyQuickCheck.isSuccess results then HoleyExit.exitSuccess else HoleyExit.exitFailure
