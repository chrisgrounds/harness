{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Core.RunTime
import Core.Run
import Test.QuickCheck
import qualified Test.QuickCheck as HoleyQuickCheck
import qualified System.Exit as HoleyExit
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as Text

instance Arbitrary RunTimestamp where
  arbitrary = RunTimestamp <$> arbitrary

instance Arbitrary RunTiming where
  arbitrary = oneof [pure TimingUnknown, TimingRunning <$> arbitrary <*> arbitrary, TimingFinished <$> arbitrary <*> arbitrary]

prop_freezingStopsOfflineTime :: RunTimestamp -> RunTimestamp -> RunTimestamp -> RunTimestamp -> Bool
prop_freezingStopsOfflineTime start sampled now later =
  let frozen = freezeRunTiming (TimingRunning start sampled)
  in elapsedRunTime now frozen == elapsedRunTime sampled (TimingRunning start sampled)
     && elapsedRunTime now frozen == elapsedRunTime later frozen
     && runTimeLabel now frozen == runTimeLabel later frozen

prop_freezingIdempotent :: RunTiming -> Bool
prop_freezingIdempotent t = freezeRunTiming (freezeRunTiming t) == freezeRunTiming t

prop_freezingPreservesTerminal :: RunTimestamp -> RunTimestamp -> Bool
prop_freezingPreservesTerminal start end = freezeRunTiming TimingUnknown == TimingUnknown && freezeRunTiming (TimingFinished start end) == TimingFinished start end

prop_freezingUsesTotalLabel :: Bool
prop_freezingUsesTotalLabel = runTimeLabel (RunTimestamp 99999999) (freezeRunTiming (TimingRunning (RunTimestamp 1000) (RunTimestamp 66000))) == Just (Text.pack "Total time: 1m 5s")

prop_finishedTimeIsFrozen :: RunTimestamp -> RunTimestamp -> RunTimestamp -> RunTimestamp -> Bool
prop_finishedTimeIsFrozen start end now later =
  elapsedRunTime now (TimingFinished start end) == elapsedRunTime later (TimingFinished start end)
  && runTimeLabel now (TimingFinished start end) == runTimeLabel later (TimingFinished start end)

prop_liveTimeAdvances :: Integer -> NonNegative Integer -> NonNegative Integer -> Bool
prop_liveTimeAdvances start (NonNegative elapsed) (NonNegative advance) =
  let timing = TimingRunning (RunTimestamp start) (RunTimestamp start)
  in elapsedRunTime (RunTimestamp (start + elapsed)) timing == Just (RunDuration elapsed)
     && elapsedRunTime (RunTimestamp (start + elapsed + advance)) timing == Just (RunDuration (elapsed + advance))

prop_durationNeverNegative :: RunTimestamp -> RunTiming -> Bool
prop_durationNeverNegative now timing = maybe True ((>= 0) . runDurationMilliseconds) (elapsedRunTime now timing)

prop_unknownTimingHidden :: RunTimestamp -> Bool
prop_unknownTimingHidden now = elapsedRunTime now TimingUnknown == Nothing && runTimeLabel now TimingUnknown == Nothing

prop_timingRoundTrip :: RunTiming -> Bool
prop_timingRoundTrip timing = A.decode (A.encode timing) == Just timing

prop_savedRunTimingRoundTrip :: RunTiming -> Bool
prop_savedRunTimingRoundTrip timing = let r = (newRun (Requirement Text.empty)) {runTiming = timing} in A.decode (A.encode r) == Just r

prop_legacyRunHasUnknownTiming :: Bool
prop_legacyRunHasUnknownTiming =
  let r = newRun (Requirement Text.empty)
  in case A.toJSON r of
       A.Object fields -> A.fromJSON (A.Object (KM.delete (K.fromString "runTiming") fields)) == A.Success r
       _ -> False

prop_oldestRunDefaultsBothFields :: Bool
prop_oldestRunDefaultsBothFields =
  let r = newRun (Requirement Text.empty)
  in case A.toJSON r of
       A.Object fields -> A.fromJSON (A.Object (KM.delete (K.fromString "activeToolCalls") (KM.delete (K.fromString "runTiming") fields))) == A.Success r
       _ -> False

prop_explicitTimingSurvivesLegacyToolDefault :: RunTiming -> Bool
prop_explicitTimingSurvivesLegacyToolDefault timing =
  let r = (newRun (Requirement Text.empty)) {runTiming = timing}
  in case A.toJSON r of
       A.Object fields -> A.fromJSON (A.Object (KM.delete (K.fromString "activeToolCalls") fields)) == A.Success r
       _ -> False

prop_retryPreservesTiming :: RunTiming -> Bool
prop_retryPreservesTiming timing =
  let r = (newRun (Requirement Text.empty)) {runTiming = timing}
  in all (\step -> runTiming (restartFrom step r) == timing && runTiming (setStep step (Failed Text.empty) r) == timing) allSteps

prop_formatBoundaries :: Bool
prop_formatBoundaries = all (\(ms, expected) -> formatRunDuration (RunDuration ms) == Text.pack expected)
  [(-1,"0s"),(0,"0s"),(999,"0s"),(1000,"1s"),(59999,"59s"),(60000,"1m 0s"),(3599999,"59m 59s"),(3600000,"1h 0m 0s"),(3661000,"1h 1m 1s"),(90000000,"25h 0m 0s")]

prop_labelsDistinguishLiveAndTotal :: Bool
prop_labelsDistinguishLiveAndTotal =
  runTimeLabel (RunTimestamp 65000) (TimingRunning (RunTimestamp 0) (RunTimestamp 0)) == Just (Text.pack "Elapsed: 1m 5s")
  && runTimeLabel (RunTimestamp 99000) (TimingFinished (RunTimestamp 0) (RunTimestamp 65000)) == Just (Text.pack "Total time: 1m 5s")

main :: IO ()
main = do
  results <-
    sequence
      [ putStrLn "=== prop_freezingStopsOfflineTime" >> HoleyQuickCheck.quickCheckResult prop_freezingStopsOfflineTime
      , putStrLn "=== prop_freezingIdempotent" >> HoleyQuickCheck.quickCheckResult prop_freezingIdempotent
      , putStrLn "=== prop_freezingPreservesTerminal" >> HoleyQuickCheck.quickCheckResult prop_freezingPreservesTerminal
      , putStrLn "=== prop_freezingUsesTotalLabel" >> HoleyQuickCheck.quickCheckResult prop_freezingUsesTotalLabel
      , putStrLn "=== prop_finishedTimeIsFrozen" >> HoleyQuickCheck.quickCheckResult prop_finishedTimeIsFrozen
      , putStrLn "=== prop_liveTimeAdvances" >> HoleyQuickCheck.quickCheckResult prop_liveTimeAdvances
      , putStrLn "=== prop_durationNeverNegative" >> HoleyQuickCheck.quickCheckResult prop_durationNeverNegative
      , putStrLn "=== prop_unknownTimingHidden" >> HoleyQuickCheck.quickCheckResult prop_unknownTimingHidden
      , putStrLn "=== prop_timingRoundTrip" >> HoleyQuickCheck.quickCheckResult prop_timingRoundTrip
      , putStrLn "=== prop_savedRunTimingRoundTrip" >> HoleyQuickCheck.quickCheckResult prop_savedRunTimingRoundTrip
      , putStrLn "=== prop_legacyRunHasUnknownTiming" >> HoleyQuickCheck.quickCheckResult prop_legacyRunHasUnknownTiming
      , putStrLn "=== prop_oldestRunDefaultsBothFields" >> HoleyQuickCheck.quickCheckResult prop_oldestRunDefaultsBothFields
      , putStrLn "=== prop_explicitTimingSurvivesLegacyToolDefault" >> HoleyQuickCheck.quickCheckResult prop_explicitTimingSurvivesLegacyToolDefault
      , putStrLn "=== prop_retryPreservesTiming" >> HoleyQuickCheck.quickCheckResult prop_retryPreservesTiming
      , putStrLn "=== prop_formatBoundaries" >> HoleyQuickCheck.quickCheckResult prop_formatBoundaries
      , putStrLn "=== prop_labelsDistinguishLiveAndTotal" >> HoleyQuickCheck.quickCheckResult prop_labelsDistinguishLiveAndTotal
      ]
  if all HoleyQuickCheck.isSuccess results then HoleyExit.exitSuccess else HoleyExit.exitFailure
