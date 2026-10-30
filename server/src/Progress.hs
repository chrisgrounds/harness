module Progress (Progress, newProgress, update, tryUpdate, readRun, publishEvent) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Core (Run, ServerEvent (..))

data Progress = Progress (MVar Run) (ServerEvent -> IO ())

newProgress :: (ServerEvent -> IO ()) -> Run -> IO Progress
newProgress publish run = do
  publish (RunUpdated run)
  state <- newMVar run
  pure (Progress state publish)

update :: Progress -> (Run -> Run) -> IO ()
update (Progress state publish) f = modifyMVar_ state $ \run -> do
  let run' = f run
  publish (RunUpdated run')
  pure run'

tryUpdate :: Progress -> (Run -> Either e Run) -> IO (Either e ())
tryUpdate (Progress state publish) f = modifyMVar state $ \run ->
  case f run of
    Left err -> pure (run, Left err)
    Right run' -> publish (RunUpdated run') >> pure (run', Right ())

readRun :: Progress -> IO Run
readRun (Progress state _) = readMVar state

publishEvent :: Progress -> ServerEvent -> IO ()
publishEvent (Progress _ publish) ev = publish ev
