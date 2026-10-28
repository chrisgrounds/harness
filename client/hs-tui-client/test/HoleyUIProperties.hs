{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import UI
import Core.Event
import Test.QuickCheck
import qualified Test.QuickCheck as HoleyQuickCheck
import qualified System.Exit as HoleyExit
import qualified Data.Text as T
import Data.Aeson (encode, decode)
import Core (Model, allModels, modelId)

newtype AnyModel = AnyModel Model deriving (Show)

instance Arbitrary AnyModel where
  arbitrary = AnyModel <$> elements allModels

countText :: Int -> T.Text
countText n = T.pack (show n) <> T.pack (if n == 1 then " run" else " runs")

prop_modelNameLeftOfRunCount :: AnyModel -> NonNegative Int -> Bool
prop_modelNameLeftOfRunCount (AnyModel m) (NonNegative n) =
  runsSummary Nothing (Just m) n == modelId m <> T.pack " \183 " <> runsSummary Nothing Nothing n

prop_unknownModelShowsOnlyCount :: NonNegative Int -> Bool
prop_unknownModelShowsOnlyCount (NonNegative n) =
  runsSummary Nothing Nothing n == countText n

prop_modelSummaryStartsWithModelAndEndsWithCount :: AnyModel -> NonNegative Int -> Bool
prop_modelSummaryStartsWithModelAndEndsWithCount (AnyModel m) (NonNegative n) =
  let s = runsSummary Nothing (Just m) n
   in modelId m `T.isPrefixOf` s && countText n `T.isSuffixOf` s

prop_connectedRoundTripsJSON :: AnyModel -> Bool
prop_connectedRoundTripsJSON (AnyModel m) =
  decode (encode (Connected m)) == Just (Connected m)

prop_connectedDistinctFromOtherEvents :: AnyModel -> Bool
prop_connectedDistinctFromOtherEvents (AnyModel m) =
  (decode (encode (Connected m)) :: Maybe ServerEvent) /= Just TurnFinished

main :: IO ()
main = do
  results <-
    sequence
      [ putStrLn "=== prop_modelNameLeftOfRunCount" >> HoleyQuickCheck.quickCheckResult prop_modelNameLeftOfRunCount
      , putStrLn "=== prop_unknownModelShowsOnlyCount" >> HoleyQuickCheck.quickCheckResult prop_unknownModelShowsOnlyCount
      , putStrLn "=== prop_modelSummaryStartsWithModelAndEndsWithCount" >> HoleyQuickCheck.quickCheckResult prop_modelSummaryStartsWithModelAndEndsWithCount
      , putStrLn "=== prop_connectedRoundTripsJSON" >> HoleyQuickCheck.quickCheckResult prop_connectedRoundTripsJSON
      , putStrLn "=== prop_connectedDistinctFromOtherEvents" >> HoleyQuickCheck.quickCheckResult prop_connectedDistinctFromOtherEvents
      ]
  if all HoleyQuickCheck.isSuccess results then HoleyExit.exitSuccess else HoleyExit.exitFailure
