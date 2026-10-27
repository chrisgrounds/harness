{-# LANGUAGE OverloadedStrings #-}

-- | A scripted run that exercises the step protocol and client without calling
-- the model. The first iteration fails a property and loops back to
-- implementation; the second passes.
module FakeRun (fakeRun) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Core
import Progress (Progress, update)

fakeRun :: Progress -> IO ()
fakeRun progress = do
  mapM_ passStep [DesignTypes, DesignSignatures, WriteProperties, CheckSkeleton]
  implementAll
  passStep TypeCheck
  failStep RunProperties "prop_mergeAssociative failed after 12 tests"
  update progress (\run -> (restartFrom Implement run) {history = ["Iteration 1: properties failed: prop_mergeAssociative, so reimplementing merge."]})
  implementAll
  passStep TypeCheck
  passStep RunProperties
  where
    names = map FunctionName ["parseLine", "normalise", "merge", "render", "validate"]

    passStep step = do
      update progress (setStep step Running)
      pause 2
      update progress (setStep step Passed)

    failStep step reason = do
      update progress (setStep step Running)
      pause 2
      update progress (setStep step (Failed reason))
      pause 3

    implementAll = do
      update progress (\run -> foldl (\r n -> setTask n Queued r) (setStep Implement Running run) names)
      forConcurrently_ (zip [1 ..] names) $ \(i, name) -> do
        pause i
        update progress (setTask name Working)
        pause (2 + i `mod` 3)
        update progress (setTask name Done)
      update progress (setStep Implement Passed)

-- | Half-second units.
pause :: Int -> IO ()
pause n = threadDelay (n * 500000)
