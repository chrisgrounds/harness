{-# LANGUAGE OverloadedStrings #-}

-- | Text: passing it to Dear ImGui, measuring it, and setting it in a colour.
module UI.Text
  ( withText,
    textWidth,
    ellipsize,
    label,
    paragraph,
    rightAligned,
    rightAt,
  )
where

import Control.Exception (bracket_)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import DearImGui (ImVec2 (..), getCursorPosX, setCursorPosX)
import qualified DearImGui.Raw as Raw
import Foreign (nullPtr)
import Foreign.C.String (CString)
import Theme.Palette (Colour)
import UI.Style (withColourOf)

withText :: Text -> (CString -> IO a) -> IO a
withText = B.useAsCString . encodeUtf8

-- | The text's width in the current font.
textWidth :: Text -> IO Float
textWidth t = do
  ImVec2 width _ <- withText t $ \p -> Raw.calcTextSize p nullPtr 0 (-1)
  pure width

-- | The first line of the text, cut short with an ellipsis to fit the width
-- in the current font.
ellipsize :: Float -> Text -> IO Text
ellipsize width t = do
  let first = T.takeWhile (/= '\n') t
      cut = first /= t
  full <- textWidth first
  if full <= width && not cut
    then pure first
    else go first
  where
    go s
      | T.null s = pure "…"
      | otherwise = do
          let shorter = T.stripEnd (T.dropEnd 1 s)
              candidate = shorter <> "…"
          fits <- (<= width) <$> textWidth candidate
          if fits then pure candidate else go shorter

-- | Text in a colour, on one line.
label :: Colour -> Text -> IO ()
label c t = withColourOf c (withText t (`Raw.textUnformatted` Nothing))

-- | Text in a colour, wrapped at the given position within the window, or
-- its edge with 0. Every line starts where the first does, so it hangs
-- beside an icon.
paragraph :: Float -> Colour -> Text -> IO ()
paragraph wrapAt c t = bracket_ (Raw.pushTextWrapPos (realToFrac wrapAt)) Raw.popTextWrapPos (label c t)

-- | Text in a colour at the right end of the current line.
rightAligned :: Colour -> Text -> IO ()
rightAligned c t = do
  Raw.sameLine
  ImVec2 avail _ <- Raw.getContentRegionAvail
  left <- getCursorPosX
  endingAt (left + avail) c t

-- | Text in a colour ending at the given position on the current line.
rightAt :: Float -> Colour -> Text -> IO ()
rightAt edge c t = Raw.sameLine >> endingAt edge c t

-- | Text ending at the position, or starting at the cursor if it is too
-- wide to.
endingAt :: Float -> Colour -> Text -> IO ()
endingAt edge c t = do
  width <- textWidth t
  here <- getCursorPosX
  setCursorPosX (max here (edge - width))
  label c t
