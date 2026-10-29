-- | Values that glide to where they are going rather than jump, so the
-- interface moves smoothly in a frame-by-frame immediate-mode loop.
module UI.Motion (Motion, newMotion, startFrame, elapsed, ease, easeFrom, keepMoving, moving) where

import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified DearImGui.Raw as Raw

data Motion = Motion
  { values :: IORef (Map.Map Text Float),
    lastTime :: IORef (Maybe Double),
    -- | Seconds since the last frame.
    delta :: IORef Float,
    -- | Whether anything is still on its way this frame.
    unsettled :: IORef Bool
  }

newMotion :: IO Motion
newMotion = Motion <$> newIORef Map.empty <*> newIORef Nothing <*> newIORef 0 <*> newIORef False

-- | Measures the time since the last frame. Call once a frame, after
-- Dear ImGui's new frame. A long gap, like waiting for input, counts as one
-- ordinary frame, so nothing leaps.
startFrame :: Motion -> IO ()
startFrame m = do
  now <- Raw.getTime
  before <- readIORef (lastTime m)
  writeIORef (lastTime m) (Just now)
  writeIORef (delta m) (maybe 0 (\t -> realToFrac (min (1 / 30) (now - t))) before)
  writeIORef (unsettled m) False

-- | Seconds since the last frame.
elapsed :: Motion -> IO Float
elapsed = readIORef . delta

-- | The value called @key@, moved toward the target with the given time
-- constant in seconds: about two thirds of the way there after that long.
-- A key seen for the first time starts at its target.
ease :: Motion -> Text -> Float -> Float -> IO Float
ease m key tau target = do
  start <- Map.findWithDefault target key <$> readIORef (values m)
  glide m key tau start target

-- | Like 'ease', but a key seen for the first time starts at the given value.
easeFrom :: Motion -> Text -> Float -> Float -> Float -> IO Float
easeFrom m key tau initial target = do
  start <- Map.findWithDefault initial key <$> readIORef (values m)
  glide m key tau start target

glide :: Motion -> Text -> Float -> Float -> Float -> IO Float
glide m key tau start target = do
  dt <- readIORef (delta m)
  let next = target + (start - target) * exp (negate dt / tau)
      settled = abs (next - target) < 0.002 * max 1 (abs target)
      value = if settled then target else next
  modifyIORef' (values m) (Map.insert key value)
  if settled then pure () else keepMoving m
  pure value

-- | Asks for another frame straight away, for something still moving.
keepMoving :: Motion -> IO ()
keepMoving m = writeIORef (unsettled m) True

-- | Whether anything asked for another frame this frame.
moving :: Motion -> IO Bool
moving = readIORef . unsettled
