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
    styleColourU32,
    setMouseCursor,
    getFramePadding,
    beginClosableModal,
    beginComboWithFlags,
  )
where

import Control.Exception (bracket_)
import Control.Monad (unless)
import DearImGui (ImGuiCol (..), ImGuiComboFlags (..), ImGuiKey (..), ImGuiMouseCursor (..), ImGuiStyleVar (..), ImGuiWindowFlags (..), ImVec2 (..), pattern ImGuiStyleVar_Alpha, pattern ImGuiMouseCursor_Hand)
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

foreign import ccall unsafe "harness_set_metrics" c_setMetrics :: CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "harness_push_style_float" c_pushStyleFloat :: CInt -> CFloat -> IO ()

foreign import ccall unsafe "harness_colour" c_colour :: CFloat -> CFloat -> CFloat -> CFloat -> IO Word32

handCursor :: IO ()
handCursor = setMouseCursor ImGuiMouseCursor_Hand

foreign import ccall unsafe "harness_scroll_y" c_scrollY :: IO CFloat

foreign import ccall unsafe "harness_scroll_max_y" c_scrollMaxY :: IO CFloat

foreign import ccall unsafe "harness_set_scroll_y" c_setScrollY :: CFloat -> IO ()

foreign import ccall unsafe "harness_set_scroll_here_y" c_setScrollHereY :: CFloat -> IO ()

foreign import ccall unsafe "harness_chord_pressed" c_chordPressed :: CInt -> IO CInt

foreign import ccall unsafe "harness_take_chord" c_takeChord :: CInt -> IO CInt

foreign import ccall unsafe "harness_composer"
  c_composer :: CString -> Ptr CChar -> CInt -> CFloat -> CFloat -> CInt -> CString -> Ptr CInt -> IO CInt

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
    (f (rounding m)) -- window
    (f (rounding m)) -- child
    (f (rounding m)) -- popup
    (f (rounding m * 0.75)) -- frame
    (f (rounding m * 0.75)) -- grab
    (f (rounding m)) -- scrollbar
    (f (scrollbarSize m))
    0 -- window border
    1 -- child border
    1 -- frame border
    1 -- popup border
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

-- | The raw composer: label, buffer and its size, width, height,
-- whether to take the keyboard, and a replacement or 'Nothing'. Returns
-- whether Enter submitted and whether the replacement was made.
composerBox :: CString -> Ptr CChar -> Int -> Float -> Float -> Bool -> CString -> IO (Bool, Bool)
composerBox label buf size width height focus replacement =
  alloca $ \replaced -> do
    submitted <- c_composer label buf (fromIntegral size) (realToFrac width) (realToFrac height) (if focus then 1 else 0) replacement replaced
    made <- peek replaced
    pure (submitted /= 0, made /= 0)

-- Primitive binding extensions only: component style and layout live in UI.Widgets.
foreign import ccall unsafe "harness_style_colour"
  c_styleColour :: CInt -> IO Word32
foreign import ccall unsafe "harness_mouse_cursor"
  c_mouseCursor :: CInt -> IO ()
foreign import ccall unsafe "harness_frame_padding"
  c_framePadding :: Ptr CFloat -> Ptr CFloat -> IO ()
foreign import ccall unsafe "harness_begin_closable_modal"
  c_beginClosableModal :: CString -> Ptr CInt -> CInt -> IO CInt
foreign import ccall unsafe "harness_begin_combo"
  c_beginCombo :: CString -> CString -> CInt -> IO CInt

-- | The current style colour, including the style's opacity.
styleColourU32 :: ImGuiCol -> IO Word32
styleColourU32 (ImGuiCol col) = c_styleColour col

setMouseCursor :: ImGuiMouseCursor -> IO ()
setMouseCursor (ImGuiMouseCursor cursor) = c_mouseCursor cursor

getFramePadding :: IO ImVec2
getFramePadding = alloca $ \px -> alloca $ \py -> do
  c_framePadding px py
  ImVec2 <$> (realToFrac <$> peek px) <*> (realToFrac <$> peek py)

-- | Begin a modal with a close button. EndPopup is needed only when visible.
beginClosableModal :: CString -> ImGuiWindowFlags -> IO (Bool, Bool)
beginClosableModal title (ImGuiWindowFlags flags') = alloca $ \ptr -> do
  poke ptr 1
  visible <- c_beginClosableModal title ptr flags'
  open <- peek ptr
  pure (visible /= 0, open /= 0)

-- | Begin a combo with flags. EndCombo is needed only when open.
beginComboWithFlags :: CString -> CString -> ImGuiComboFlags -> IO Bool
beginComboWithFlags label preview (ImGuiComboFlags flags') = (/= 0) <$> c_beginCombo label preview flags'
