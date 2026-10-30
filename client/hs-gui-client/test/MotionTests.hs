module MotionTests (motionTests) where

import Control.Monad (forM, forM_)
import UI.Motion

motionTests :: IO Bool
motionTests = and <$> sequence
  [ check "measurement and drawing sample one animation value per frame" duplicateSamples,
    check "run and chat animations remain independent and settle" independentRuns,
    check "new runs have their own appearance animation" independentAppearance,
    check "settled animations resume when their target changes" changingTarget,
    check "frame time is bounded after waiting or clock changes" boundedTime
  ]
  where
    check name test = do
      passed <- test
      putStrLn (name ++ if passed then ": OK" else ": FAILED")
      pure passed

duplicateSamples :: IO Bool
duplicateSamples = do
  m <- newMotion
  startFrame m 0
  _ <- ease m ComposerHeight 0.07 100
  startFrame m (1 / 60)
  measured <- ease m ComposerHeight 0.07 200
  drawn <- ease m ComposerHeight 0.07 200
  startFrame m (2 / 60)
  next <- ease m ComposerHeight 0.07 200
  pure (measured == drawn && measured > 100 && measured < next && next < 200)

-- Repeated requirement text can occur in any of these runs. Distinct keys
-- allow their opposing targets to settle rather than advance one value in
-- both directions forever.
independentRuns :: IO Bool
independentRuns = do
  m <- newMotion
  let targets = [(RunRail 0 0 0, 1), (RunRail 0 1 0, 0), (RunRail 1 0 0, 0.5)]
  forM_ [0 .. 120 :: Int] $ \frame -> do
    startFrame m (fromIntegral frame / 60)
    forM_ targets $ \(key, target) -> do
      _ <- easeFrom m key 0.14 (1 - target) target
      pure ()
  final <- forM targets $ \(key, target) -> (== target) <$> ease m key 0.14 target
  busy <- moving m
  pure (and final && not busy)

independentAppearance :: IO Bool
independentAppearance = do
  m <- newMotion
  forM_ [0 .. 90 :: Int] $ \frame -> do
    startFrame m (fromIntegral frame / 60)
    _ <- easeFrom m (RunAppearance 0 0) 0.09 0 1
    pure ()
  old <- easeFrom m (RunAppearance 0 0) 0.09 0 1
  sameChat <- easeFrom m (RunAppearance 0 1) 0.09 0 1
  otherChat <- easeFrom m (RunAppearance 1 0) 0.09 0 1
  pure (old == 1 && sameChat < 1 && otherChat < 1)

changingTarget :: IO Bool
changingTarget = do
  m <- newMotion
  startFrame m 0
  initial <- ease m (RunProgress 0 0) 0.12 1
  idle <- not <$> moving m
  startFrame m (1 / 60)
  restarted <- ease m (RunProgress 0 0) 0.12 0
  busy <- moving m
  pure (initial == 1 && idle && restarted > 0 && restarted < 1 && busy)

boundedTime :: IO Bool
boundedTime = do
  m <- newMotion
  startFrame m 10
  initial <- elapsed m
  startFrame m 100
  resumed <- elapsed m
  startFrame m 99
  backwards <- elapsed m
  pure (initial == 0 && resumed > 0 && resumed <= 1 / 30 && backwards == 0)
