{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Client.Tokens
import UI
import Test.QuickCheck
import qualified Test.QuickCheck as HoleyQuickCheck
import qualified System.Exit as HoleyExit
import Core (Model (..), allModels)
import qualified Data.Text as T

statsOf :: [Double] -> TokenStats
statsOf = foldl addRate emptyTokenStats

addOnes :: TokenStats -> Int -> TokenStats
addOnes s _ = addRate s 1

expectedMean :: [Double] -> Double
expectedMean rs = sum rs / fromIntegral (length rs)

newtype AnyModel = AnyModel Model deriving (Show)

instance Arbitrary AnyModel where
  arbitrary = AnyModel <$> elements allModels

newtype PosRateList = PosRateList [Positive Double] deriving (Show)

instance Arbitrary PosRateList where
  arbitrary = PosRateList <$> listOf1 arbitrary

prop_averageEmpty :: Bool
prop_averageEmpty = averageTokens emptyTokenStats == Nothing

prop_addIncrementsCount :: NonNegative Int -> Bool
prop_addIncrementsCount (NonNegative n) =
  sampleCount (foldl addOnes emptyTokenStats [1 .. n]) == n

prop_averageIsMean :: PosRateList -> Bool
prop_averageIsMean (PosRateList rss) =
  case averageTokens (statsOf (map getPositive rss)) of
    Just a -> abs (a - expectedMean (map getPositive rss)) < 1e-6
    Nothing -> False

prop_rateLeftOfModel :: AnyModel -> NonNegative Int -> Bool
prop_rateLeftOfModel (AnyModel m) (NonNegative n) =
  runsSummary (Just 25) (Just m) n == T.pack "25.0 tok/s  " <> runsSummary Nothing (Just m) n
    && runsSummary (Just 25) (Just ClaudeOpus) 3 == T.pack "25.0 tok/s  claude-opus-5-5  \183  3 runs"

main :: IO ()
main = do
  results <-
    sequence
      [ putStrLn "=== prop_averageEmpty" >> HoleyQuickCheck.quickCheckResult prop_averageEmpty
      , putStrLn "=== prop_addIncrementsCount" >> HoleyQuickCheck.quickCheckResult prop_addIncrementsCount
      , putStrLn "=== prop_averageIsMean" >> HoleyQuickCheck.quickCheckResult prop_averageIsMean
      , putStrLn "=== prop_rateLeftOfModel" >> HoleyQuickCheck.quickCheckResult prop_rateLeftOfModel
      ]
  if all HoleyQuickCheck.isSuccess results then HoleyExit.exitSuccess else HoleyExit.exitFailure
