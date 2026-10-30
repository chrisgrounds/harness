-- | Session-owned animation values, independent of the windowing backend.
-- The caller supplies the frame time; reading the same animation twice in a
-- frame returns the same value, so measurement and drawing agree.
module UI.Motion
  ( Motion, MotionKey (..), newMotion, startFrame, elapsed,
    ease, easeFrom, keepMoving, moving
  ) where

import Data.IORef
import qualified Data.Map.Strict as Map

-- | Run keys contain the chat ID and the run's append-only ordinal, oldest
-- first. Requirements and the currently selected chat do not affect identity.
-- A rail also identifies its segment. Progress reuses one value across
-- iterations rather than retaining an entry for each attempt.
data MotionKey
  = ComposerHeight
  | ComposerFocus
  | RunAppearance Int Int
  | RunRail Int Int Int
  | RunProgress Int Int
  deriving (Eq, Ord, Show)

data Motion = Motion
  { values :: IORef (Map.Map MotionKey Float),
    sampled :: IORef (Map.Map MotionKey Float),
    lastTime :: IORef (Maybe Double),
    delta :: IORef Float,
    unsettled :: IORef Bool
  }

-- | Values live as long as the session's chats and runs. The key space has
-- a fixed number of values per run, even when that run retries repeatedly.
newMotion :: IO Motion
newMotion = Motion <$> newIORef Map.empty <*> newIORef Map.empty
  <*> newIORef Nothing <*> newIORef 0 <*> newIORef False

-- | Begins a frame at the supplied time in seconds. A long gap, such as
-- waiting for input, counts as one ordinary frame so nothing leaps.
startFrame :: Motion -> Double -> IO ()
startFrame m now = do
  before <- readIORef (lastTime m)
  writeIORef (lastTime m) (Just now)
  writeIORef (delta m) (maybe 0 (\t -> realToFrac (max 0 (min (1 / 30) (now - t)))) before)
  writeIORef (sampled m) Map.empty
  writeIORef (unsettled m) False

-- | Seconds to advance animations by in this frame.
elapsed :: Motion -> IO Float
elapsed = readIORef . delta

-- | Moves toward the target with the given time constant in seconds. A new
-- key starts at its target. The first sample determines the value for the
-- entire frame; subsequent samples do not advance it again.
ease :: Motion -> MotionKey -> Float -> Float -> IO Float
ease m key tau target = sample m key tau target target

-- | Like 'ease', but a new key starts at the supplied initial value.
easeFrom :: Motion -> MotionKey -> Float -> Float -> Float -> IO Float
easeFrom = sample

sample :: Motion -> MotionKey -> Float -> Float -> Float -> IO Float
sample m key tau initial target = do
  cached <- Map.lookup key <$> readIORef (sampled m)
  case cached of
    Just value -> pure value
    Nothing -> do
      start <- Map.findWithDefault initial key <$> readIORef (values m)
      dt <- readIORef (delta m)
      let next = if tau <= 0 then target else target + (start - target) * exp (negate dt / tau)
          settled = abs (next - target) < 0.002 * max 1 (abs target)
          value = if settled then target else next
      modifyIORef' (values m) (Map.insert key value)
      modifyIORef' (sampled m) (Map.insert key value)
      if settled then pure () else keepMoving m
      pure value

-- | Asks for another frame straight away, for something still moving.
keepMoving :: Motion -> IO ()
keepMoving m = writeIORef (unsettled m) True

-- | Whether any animation asked for another frame.
moving :: Motion -> IO Bool
moving = readIORef . unsettled
