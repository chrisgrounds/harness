{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.RunTime where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T

newtype RunTimestamp = RunTimestamp { runTimestampMilliseconds :: Integer }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype RunDuration = RunDuration { runDurationMilliseconds :: Integer }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data RunTiming
  = TimingUnknown
  | TimingRunning { timingStarted :: RunTimestamp, timingSampled :: RunTimestamp }
  | TimingFinished { timingStarted :: RunTimestamp, timingEnded :: RunTimestamp }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

freezeRunTiming :: RunTiming -> RunTiming
freezeRunTiming (TimingRunning start sampled) = TimingFinished start (max start sampled)
freezeRunTiming timing = timing

elapsedRunTime :: RunTimestamp -> RunTiming -> Maybe RunDuration
elapsedRunTime _ TimingUnknown = Nothing
elapsedRunTime (RunTimestamp now) (TimingRunning (RunTimestamp start) _) =
  Just (RunDuration (max 0 (now - start)))
elapsedRunTime _ (TimingFinished (RunTimestamp start) (RunTimestamp end)) =
  Just (RunDuration (max 0 (end - start)))

formatRunDuration :: RunDuration -> Text
formatRunDuration (RunDuration milliseconds)
  | seconds < 60 = render seconds <> "s"
  | seconds < 3600 = render minutes <> "m " <> render remainingSeconds <> "s"
  | otherwise = render hours <> "h " <> render remainingMinutes <> "m " <> render remainingSeconds <> "s"
  where
    seconds = max 0 milliseconds `div` 1000
    (minutes, remainingSeconds) = seconds `divMod` 60
    (hours, remainingMinutes) = minutes `divMod` 60
    render = T.pack . show

runTimeLabel :: RunTimestamp -> RunTiming -> Maybe Text
runTimeLabel _ TimingUnknown = Nothing
runTimeLabel now timing@(TimingRunning _ _) =
  (\duration -> "Elapsed: " <> formatRunDuration duration) <$> elapsedRunTime now timing
runTimeLabel now timing@(TimingFinished _ _) =
  (\duration -> "Total time: " <> formatRunDuration duration) <$> elapsedRunTime now timing

