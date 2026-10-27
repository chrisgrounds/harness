module Main (main) where

import Core (newRun)
import FakeRun (fakeRun)
import Progress (newProgress)
import qualified Socket

main :: IO ()
main = do
  let socketPath = "/tmp/harness.sock"
  putStrLn "opening server, listening..."

  -- Every requirement starts a run. Runs are scripted until the steps are
  -- backed by the model (see Agent).
  Socket.serve socketPath $ \publish req ->
    newProgress publish (newRun req) >>= fakeRun
