{-# LANGUAGE ForeignFunctionInterface #-}

module TerminalWidth (installTerminalWidths) where

import Control.Monad (unless)
import Data.Char (ord)
import Foreign.C.Types (CInt (..), CWchar (..))
import qualified Graphics.Vty as V
import Graphics.Vty.UnicodeWidthTable.Install (installUnicodeWidthTable, isCustomTableReady)
import Graphics.Vty.UnicodeWidthTable.Query (buildUnicodeWidthTable)

foreign import ccall unsafe "wcwidth" terminalCharWidth :: CWchar -> IO CInt

-- Vty's built-in Unicode 5 table counts modern emoji as one column. Use
-- the system's current character widths so wrapping and borders agree
-- with a UTF-8 terminal. Keep any terminal-specific table loaded by Vty.
installTerminalWidths :: IO ()
installTerminalWidths = do
  installed <- isCustomTableReady
  unless installed $ do
    table <- buildUnicodeWidthTable charWidth maxBound
    installUnicodeWidthTable table
  where
    charWidth c = do
      width <- terminalCharWidth (fromIntegral (ord c))
      pure (if width < 0 then V.wcwidth c else fromIntegral width)
