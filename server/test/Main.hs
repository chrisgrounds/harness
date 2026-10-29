module Main (main) where

import EndToEnd (endToEndTests)
import SocketTests (socketTests)
import OpenAITests (openaiTests)
import Properties (propertyTests)
import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))
import System.Exit (exitFailure)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  failures <- concat <$> sequence [propertyTests, openaiTests, socketTests, endToEndTests]
  mapM_ (putStrLn . ("FAIL: " <>)) failures
  if null failures then putStrLn "All checks passed." else exitFailure
