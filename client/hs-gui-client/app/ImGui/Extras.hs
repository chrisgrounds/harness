{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Dear ImGui calls the dear-imgui bindings leave out, from
-- @cbits/imgui_extras.cpp@.
module ImGui.Extras
  ( ensureLayout,
    configureIO,
    setColour,
    Metrics (..),
    setMetrics,
    withAlpha,
    withStyleFloat,
    colourU32,
    handCursor,
    scrollY,
    scrollMaxY,
    setScrollY,
    setScrollHereY,
    keyPressed,
    takeKey,
    composerBox,
    navigationButton,
    resizeSidebar,
    closableModal,
    sizedButton,
    modelCombo,
  )
where

import Control.Exception (bracket_)
import Control.Monad (unless)
import DearImGui (ImGuiCol (..), ImGuiKey (..), ImGuiStyleVar (..), pattern ImGuiStyleVar_Alpha)
import qualified DearImGui.Raw as Raw
import Data.Bits ((.|.))
import Data.Word (Word32)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar, CFloat (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)

foreign import ccall unsafe "harness_layout_matches" c_layoutMatches :: IO CInt

foreign import ccall unsafe "harness_configure_io" c_configureIO :: CFloat -> IO ()

foreign import ccall unsafe "harness_set_colour" c_setColour :: CInt -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "harness_set_metrics" c_setMetrics :: CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "harness_push_style_float" c_pushStyleFloat :: CInt -> CFloat -> IO ()

foreign import ccall unsafe "harness_colour" c_colour :: CFloat -> CFloat -> CFloat -> CFloat -> IO Word32

foreign import ccall unsafe "harness_hand_cursor" handCursor :: IO ()

foreign import ccall unsafe "harness_scroll_y" c_scrollY :: IO CFloat

foreign import ccall unsafe "harness_scroll_max_y" c_scrollMaxY :: IO CFloat

foreign import ccall unsafe "harness_set_scroll_y" c_setScrollY :: CFloat -> IO ()

foreign import ccall unsafe "harness_set_scroll_here_y" c_setScrollHereY :: CFloat -> IO ()

foreign import ccall unsafe "harness_chord_pressed" c_chordPressed :: CInt -> IO CInt

foreign import ccall unsafe "harness_take_chord" c_takeChord :: CInt -> IO CInt

foreign import ccall unsafe "harness_composer"
  c_composer :: CString -> CString -> Ptr CChar -> CInt -> CFloat -> CFloat -> CInt -> CString -> Ptr CInt -> IO CInt

-- | Fails unless the extras were compiled against the Dear ImGui the
-- bindings link with.
ensureLayout :: IO ()
ensureLayout = do
  matches <- c_layoutMatches
  unless (matches /= 0) $ fail "cbits/imgui/imgui.h does not match the Dear ImGui dear-imgui was built with"

-- | Draws fonts at the given scale and keeps no @imgui.ini@.
configureIO :: Float -> IO ()
configureIO = c_configureIO . realToFrac

-- | Sets a style colour from 0-1 components.
setColour :: ImGuiCol -> Float -> Float -> Float -> Float -> IO ()
setColour (ImGuiCol col) r g b a = c_setColour col (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

-- | The style's sizes, in points.
data Metrics = Metrics
  { windowPadding :: Float,
    framePadding :: (Float, Float),
    itemSpacing :: (Float, Float),
    rounding :: Float,
    scrollbarSize :: Float
  }

setMetrics :: Metrics -> IO ()
setMetrics m =
  c_setMetrics
    (f (windowPadding m))
    (f (fst (framePadding m)))
    (f (snd (framePadding m)))
    (f (fst (itemSpacing m)))
    (f (snd (itemSpacing m)))
    (f (rounding m))
    (f (scrollbarSize m))
  where
    f = realToFrac

-- | Draws what the action draws at the given opacity, on top of any fade
-- already in place.
withAlpha :: Float -> IO a -> IO a
withAlpha = withStyleFloat ImGuiStyleVar_Alpha

-- | Draws what the action draws with a style value of one float, such as
-- a rounding.
withStyleFloat :: ImGuiStyleVar -> Float -> IO a -> IO a
withStyleFloat (ImGuiStyleVar var) value = bracket_ (c_pushStyleFloat var (realToFrac value)) (Raw.popStyleVar 1)

-- | A colour from 0-1 components for draw lists, faded like the items
-- around it.
colourU32 :: Float -> Float -> Float -> Float -> IO Word32
colourU32 r g b a = c_colour (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)

-- | The current window's scroll position.
scrollY :: IO Float
scrollY = realToFrac <$> c_scrollY

-- | The current window's largest scroll position, as of its last frame.
scrollMaxY :: IO Float
scrollMaxY = realToFrac <$> c_scrollMaxY

setScrollY :: Float -> IO ()
setScrollY = c_setScrollY . realToFrac

-- | Scrolls so the cursor is at the given fraction of the window's height.
setScrollHereY :: Float -> IO ()
setScrollHereY = c_setScrollHereY . realToFrac

-- | Whether the key, with exactly the given modifiers, was pressed this
-- frame, even while another item, like the composer, owns it.
keyPressed :: [ImGuiKey] -> ImGuiKey -> IO Bool
keyPressed mods key = (/= 0) <$> c_chordPressed (chord mods key)

-- | Like 'keyPressed', but the key goes to no other item this frame.
takeKey :: [ImGuiKey] -> ImGuiKey -> IO Bool
takeKey mods key = (/= 0) <$> c_takeChord (chord mods key)

chord :: [ImGuiKey] -> ImGuiKey -> CInt
chord mods key = foldr (\(ImGuiKey m) acc -> m .|. acc) (code key) mods
  where
    code (ImGuiKey k) = k

-- | The raw composer: label, hint, buffer and its size, width, height,
-- whether to take the keyboard, and a replacement or 'Nothing'. Returns
-- whether Enter submitted and whether the replacement was made.
composerBox :: CString -> CString -> Ptr CChar -> Int -> Float -> Float -> Bool -> CString -> IO (Bool, Bool)
composerBox label hint buf size width height focus replacement =
  alloca $ \replaced -> do
    submitted <- c_composer label hint buf (fromIntegral size) (realToFrac width) (realToFrac height) (if focus then 1 else 0) replacement replaced
    made <- peek replaced
    pure (submitted /= 0, made /= 0)

foreign import ccall unsafe "harness_icon_button"
  c_iconButton :: CString -> CString -> CInt -> IO CInt
foreign import ccall unsafe "harness_splitter"
  c_splitter :: Ptr CFloat -> CFloat -> CFloat -> CFloat -> IO CInt
foreign import ccall unsafe "harness_modal"
  c_modal :: CString -> Ptr CInt -> IO CInt
foreign import ccall unsafe "harness_sized_button"
  c_sizedButton :: CString -> CFloat -> CFloat -> IO CInt
foreign import ccall unsafe "harness_model_combo"
  c_modelCombo :: CString -> CString -> CFloat -> IO CInt

navigationButton :: CString -> CString -> Bool -> IO Bool
navigationButton name tooltip settings = (/= 0) <$> c_iconButton name tooltip (if settings then 0 else 1)

resizeSidebar :: Float -> Float -> Float -> Float -> IO Float
resizeSidebar width minimum' maximum' height = alloca $ \ptr -> do
  poke ptr (realToFrac width)
  _ <- c_splitter ptr (realToFrac minimum') (realToFrac maximum') (realToFrac height)
  realToFrac <$> peek ptr

closableModal :: CString -> IO (Bool, Bool)
closableModal title = alloca $ \ptr -> do
  poke ptr 1
  visible <- c_modal title ptr
  open <- peek ptr
  pure (visible /= 0, open /= 0)

sizedButton :: CString -> Float -> Float -> IO Bool
sizedButton label width height = (/= 0) <$> c_sizedButton label (realToFrac width) (realToFrac height)

modelCombo :: CString -> CString -> Float -> IO Bool
modelCombo label preview width = (/= 0) <$> c_modelCombo label preview (realToFrac width)
