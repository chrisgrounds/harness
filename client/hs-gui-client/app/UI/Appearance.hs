-- | The read-only visual resources shared by components.
module UI.Appearance (Appearance (..)) where

import Theme.Palette (Palette)
import UI.Fonts (Fonts)

data Appearance = Appearance
  { palette :: Palette,
    fonts :: Fonts
  }
