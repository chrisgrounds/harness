{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Fonts (Faces (..), Fonts, loadFonts) where

import Control.Monad (filterM)
import Data.Maybe (listToMaybe)
import DearImGui (Font)
import qualified DearImGui.FontAtlas as FontAtlas
import System.Directory (doesFileExist)

-- | The type scale: 13.5, 16, 18.5 and 23 points, with code in a
-- monospaced face.
data Faces a = Faces
  { -- | Steps, notes and everything else.
    body :: a,
    -- | Secondary lines: paths, the run list's details, the status.
    small :: a,
    -- | The user's requirements, a weight up from the body.
    lead :: a,
    -- | The project's name and the empty transcript's question.
    heading :: a,
    -- | Modules, functions, files and tools: code.
    mono :: a
  }
  deriving (Functor, Foldable, Traversable)

type Fonts = Faces Font

-- | A font file, and which face in it, for a collection.
data Face = Face FilePath (Maybe Int)

-- | Loads the interface's fonts, rasterised at the display's pixel density,
-- and returns the scale to draw them at. Avenir Next brings real weights;
-- elsewhere one sans-serif weight serves throughout. Without any known
-- font, every face is Dear ImGui's built-in one, unscaled.
loadFonts :: Float -> IO (Fonts, Float)
loadFonts density = do
  avenir <- doesFileExist avenirNext
  sans <- firstExisting sansFonts
  code <- firstExisting monoFonts
  let weights
        | avenir = Just (Face avenirNext (Just 7), Face avenirNext (Just 5), Face avenirNext (Just 2))
        | otherwise = (\path -> (Face path Nothing, Face path Nothing, Face path Nothing)) <$> sans
  case weights of
    Nothing -> (\fonts -> (fonts, 1)) <$> FontAtlas.rebuild (Faces d d d d d)
    Just (regular, medium, demiBold) -> do
      let monoFace = maybe regular (`Face` Nothing) code
      fonts <-
        FontAtlas.rebuild
          Faces
            { body = face regular 16,
              small = face regular 13.5,
              lead = face medium 18.5,
              heading = face demiBold 23,
              mono = face monoFace 14.5
            }
      pure (fonts, 1 / density)
  where
    d = FontAtlas.DefaultFont
    -- The renderer loads glyphs on demand, including punctuation and arrows.
    face (Face path index) size = FontAtlas.FromTTF path (Just (size * density)) (FontAtlas.fontNo <$> index)

-- | Face 7 is Regular, 5 Medium and 2 DemiBold.
avenirNext :: FilePath
avenirNext = "/System/Library/Fonts/Avenir Next.ttc"

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting paths = listToMaybe <$> filterM doesFileExist paths

sansFonts :: [FilePath]
sansFonts =
  [ "/System/Library/Fonts/SFNS.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/TTF/DejaVuSans.ttf",
    "/usr/share/fonts/dejavu/DejaVuSans.ttf",
    "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf",
    "/usr/share/fonts/noto/NotoSans-Regular.ttf",
    "C:\\Windows\\Fonts\\segoeui.ttf"
  ]

monoFonts :: [FilePath]
monoFonts =
  [ "/System/Library/Fonts/SFNSMono.ttf",
    "/System/Library/Fonts/Menlo.ttc",
    "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf",
    "/usr/share/fonts/TTF/DejaVuSansMono.ttf",
    "/usr/share/fonts/dejavu/DejaVuSansMono.ttf",
    "/usr/share/fonts/truetype/noto/NotoSansMono-Regular.ttf",
    "/usr/share/fonts/noto/NotoSansMono-Regular.ttf",
    "C:\\Windows\\Fonts\\consola.ttf"
  ]
