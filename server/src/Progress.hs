module Progress (Progress, newProgress, update) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
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
