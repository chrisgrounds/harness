{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The composer's text box, whose text Dear ImGui edits in place.
module UI.Composer.Input (Input, newInput, input) where

import Control.Monad (when)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import DearImGui hiding (end, flags, indent, size, w, x, y, z)
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.DrawList as DrawList
import Foreign
import Foreign.C.Types (CChar)
import ImGui.Extras (composerBox, getFramePadding, styleColourU32)
import UI.Text (withText)

-- | The buffer Dear ImGui edits.
newtype Input = Input (ForeignPtr CChar)

capacity :: Int
capacity = 64 * 1024

newInput :: IO Input
newInput = do
  buffer <- mallocForeignPtrBytes capacity
  withForeignPtr buffer $ \p -> poke p 0
  pure (Input buffer)

-- | Draws the text box with the given ID at the given size, showing the
-- hint while it is empty, taking the keyboard when asked, and replacing its
-- text when given a replacement. Returns whether Enter submitted, whether
-- the replacement was made, and the text.
input :: Input -> Text -> Text -> ImVec2 -> Bool -> Maybe Text -> IO (Bool, Bool, Text)
input (Input buffer) name hint (ImVec2 width height) focus replacement =
  withForeignPtr buffer $ \p -> do
    let fitted = fitUtf8 (capacity - 1) <$> replacement
    -- Shown at once if the box is not active; when it is, Dear ImGui keeps
    -- its own copy until the replacement is made.
    mapM_ (write p) fitted
    (submitted, replaced) <-
      withText name $ \name' ->
        maybe ($ nullPtr) B.useAsCString fitted $ \replacing ->
          composerBox name' p capacity width height focus replacing
    contents <- decodeUtf8With lenientDecode <$> B.packCString p
    when (T.null contents && not (T.null hint)) $ do
      ImVec2 x0 y0 <- Raw.getItemRectMin
      ImVec2 px py <- getFramePadding
      list <- Raw.getWindowDrawList
      ink <- styleColourU32 ImGuiCol_TextDisabled
      with (ImVec2 (x0 + px) (y0 + py)) $ \at ->
        withText hint $ \hintText -> DrawList.addText_ list at ink hintText nullPtr
    pure (submitted, replaced, contents)
  where
    write p bytes = B.useAsCStringLen bytes $ \(source, n) -> copyBytes p source n >> pokeByteOff p n (0 :: Word8)

-- | The longest prefix of the text's UTF-8 of at most the given length that
-- ends between characters.
fitUtf8 :: Int -> Text -> B.ByteString
fitUtf8 limit t
  | B.length bytes <= limit = bytes
  | otherwise = B.take (head [i | i <- [limit, limit - 1 .. 0], startsCharacter i]) bytes
  where
    bytes = encodeUtf8 t
    startsCharacter i = i == 0 || B.index bytes i .&. 0xC0 /= 0x80
