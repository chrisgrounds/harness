module Progress (Progress, newProgress, update, tryUpdate, readRun) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Core (Run, ServerEvent (..))

-- | The state of one run, shared by every thread working on it. Each update
-- is published as a full snapshot.
data Progress = Progress (MVar Run) (ServerEvent -> IO ())

newProgress :: (ServerEvent -> IO ()) -> Run -> IO Progress
newProgress publish run = do
  publish (RunUpdated run)
  state <- newMVar run
  pure (Progress state publish)

-- | Publishing happens while the state is held, so snapshots arrive in order.
update :: Progress -> (Run -> Run) -> IO ()
update (Progress state publish) f = modifyMVar_ state $ \run -> do
  let run' = f run
  publish (RunUpdated run')
  pure run'

-- | Applies a change that may be rejected; only accepted changes are published.
tryUpdate :: Progress -> (Run -> Either e Run) -> IO (Either e ())
tryUpdate (Progress state publish) f = modifyMVar state $ \run ->
  case f run of
    Left err -> pure (run, Left err)
    Right run' -> publish (RunUpdated run') >> pure (run', Right ())

readRun :: Progress -> IO Run
readRun (Progress state _) = readMVar state
