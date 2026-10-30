-- | Shapes on the window's draw list.
module UI.Draw
  ( colour,
    line,
    stroke,
  )
where

import DearImGui (ImDrawFlags, ImU32, ImVec2)
import qualified DearImGui.Raw as Raw
import DearImGui.Raw.DrawList (DrawList)
import qualified DearImGui.Raw.DrawList as DrawList
import Foreign (with)
import Foreign.C.Types (CFloat)
import ImGui.Extras (colourU32)
import Theme.Palette (Colour (..))

-- | A colour for draw lists at the given opacity, faded like the items
-- around it.
colour :: Float -> Colour -> IO ImU32
colour alpha (Colour r g b) = colourU32 (f r) (f g) (f b) alpha
  where
    f v = fromIntegral v / 255

-- | A straight line on the window's draw list.
line :: ImU32 -> Float -> ImVec2 -> ImVec2 -> IO ()
line c thickness from to = do
  list <- Raw.getWindowDrawList
  with from $ \a -> with to $ \b -> DrawList.addLine list a b c (realToFrac thickness)

-- | Lines joining the points in order, closed back to the first with
-- 'ImDrawFlags_Closed'.
stroke :: DrawList -> ImU32 -> CFloat -> ImDrawFlags -> [ImVec2] -> IO ()
stroke list ink thickness closed points = do
  DrawList.pathClear list
  mapM_ (`with` DrawList.pathLineTo list) points
  DrawList.pathStroke list ink thickness closed
