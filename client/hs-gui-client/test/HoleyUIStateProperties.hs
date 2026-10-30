{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import UI.State
import Client.Chats
import Test.QuickCheck
import qualified Test.QuickCheck as HoleyQuickCheck
import qualified System.Exit as HoleyExit
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Aeson (encode, decode)
import Data.Time (UTCTime(..), fromGregorian)

applyAction a u = fst (update "/test" a u)
renameTo n u = foldl (flip applyAction) u [BeginRename (currentChatId u), EditRename n, SaveRename]
testTime = UTCTime (fromGregorian 2025 1 1) 0
savedDraftChat u = chatsToSave "/test" testTime (applyAction (Chat (currentChatId u) (Edited (T.pack "draft"))) u)

prop_renameRoundTrip :: String -> Property
prop_renameRoundTrip raw = let name = T.strip (T.pack raw) in not (T.null name) ==> let u = renameTo (T.pack raw) initialUI; saved = savedDraftChat u; restored = restoreChats "/test" saved initialUI in conjoin [map customTitle saved === [Just name], map customTitle (chatsToSave "/test" testTime restored) === [Just name], renameEditor u === Nothing, decode (encode saved) === Just saved]

prop_cancelRename :: String -> Property
prop_cancelRename s = let u = applyAction (BeginRename 0) initialUI; v = applyAction CancelRename (applyAction (EditRename (T.pack s)) u) in v === initialUI

prop_blankRenameDoesNotSave :: Property
prop_blankRenameDoesNotSave = let u = applyAction (EditRename (T.pack "  \t ")) (applyAction (BeginRename 0) initialUI); v = applyAction SaveRename u in v === u

prop_deleteLastAllocatesFresh :: Positive Int -> Property
prop_deleteLastAllocatesFresh (Positive n) = let count = min n 100; u = iterate (applyAction (DeleteChat 0)) initialUI !! 1; v = iterate (\s -> applyAction (DeleteChat (currentChatId s)) s) initialUI !! count in conjoin [currentChatId u === 1, currentChatId v === count, nextChatId v === count + 1, Map.null (otherConversations v) === True, chatsToSave "/test" testTime v === []]

prop_deleteNoncurrentPreservesSelected :: Property
prop_deleteNoncurrentPreservesSelected = let u = applyAction NewChat initialUI; v = applyAction (DeleteChat 0) u in conjoin [currentConversation v === currentConversation u, currentChatId v === currentChatId u, nextChatId v === nextChatId u, Map.member 0 (otherConversations v) === False]

prop_deleteCurrentSelectsRemaining :: Property
prop_deleteCurrentSelectsRemaining = let u = applyAction NewChat (applyAction NewChat initialUI); v = applyAction (DeleteChat 2) u in conjoin [currentChatId v === 1, nextChatId v === 3, Map.member 2 (otherConversations v) === False, Map.member 1 (otherConversations v) === False, Map.member 0 (otherConversations v) === True]

prop_missingTargetsNoOp :: Property
prop_missingTargetsNoOp = conjoin [applyAction (DeleteChat (-1)) initialUI === initialUI, applyAction (BeginRename (-1)) initialUI === initialUI]

prop_renameUrgentDeletePersistent :: Property
prop_renameUrgentDeletePersistent = let u = applyAction (Chat 0 (Edited (T.pack "draft"))) initialUI; before = chatsToSave "/test" testTime u; after = chatsToSave "/test" testTime (renameTo (T.pack "Renamed") u); deleted = chatsToSave "/test" testTime (applyAction (DeleteChat 0) u) in conjoin [urgentChange before after === True, urgentChange before deleted === True, deleted === []]

main :: IO ()
main = do
  results <-
    sequence
      [ putStrLn "=== prop_renameRoundTrip" >> HoleyQuickCheck.quickCheckResult prop_renameRoundTrip
      , putStrLn "=== prop_cancelRename" >> HoleyQuickCheck.quickCheckResult prop_cancelRename
      , putStrLn "=== prop_blankRenameDoesNotSave" >> HoleyQuickCheck.quickCheckResult prop_blankRenameDoesNotSave
      , putStrLn "=== prop_deleteLastAllocatesFresh" >> HoleyQuickCheck.quickCheckResult prop_deleteLastAllocatesFresh
      , putStrLn "=== prop_deleteNoncurrentPreservesSelected" >> HoleyQuickCheck.quickCheckResult prop_deleteNoncurrentPreservesSelected
      , putStrLn "=== prop_deleteCurrentSelectsRemaining" >> HoleyQuickCheck.quickCheckResult prop_deleteCurrentSelectsRemaining
      , putStrLn "=== prop_missingTargetsNoOp" >> HoleyQuickCheck.quickCheckResult prop_missingTargetsNoOp
      , putStrLn "=== prop_renameUrgentDeletePersistent" >> HoleyQuickCheck.quickCheckResult prop_renameUrgentDeletePersistent
      ]
  if all HoleyQuickCheck.isSuccess results then HoleyExit.exitSuccess else HoleyExit.exitFailure
