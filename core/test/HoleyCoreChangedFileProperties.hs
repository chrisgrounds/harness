module Main (main) where

import Core.ChangedFile
import qualified Data.Text as T
import qualified System.Exit as HoleyExit
import qualified Test.QuickCheck as HoleyQuickCheck

prop_newFileTag :: FilePath -> Bool
prop_newFileTag path = changeTag (ChangedFile path NewFile) == T.pack ("[write] " <> path)

prop_existingFileTag :: FilePath -> Bool
prop_existingFileTag path = changeTag (ChangedFile path ExistingFile) == T.pack ("[edit] " <> path)

main :: IO ()
main = do
  results <-
    sequence
      [ putStrLn "=== prop_newFileTag" >> HoleyQuickCheck.quickCheckResult prop_newFileTag
      , putStrLn "=== prop_existingFileTag" >> HoleyQuickCheck.quickCheckResult prop_existingFileTag
      ]
  if all HoleyQuickCheck.isSuccess results then HoleyExit.exitSuccess else HoleyExit.exitFailure
