module Main (main) where

import EndToEnd (endToEndTests)
import System.Exit (exitFailure)

main :: IO ()
main = do
  failures <- endToEndTests
  mapM_ (putStrLn . ("FAIL: " <>)) failures
  if null failures then putStrLn "All checks passed." else exitFailure
