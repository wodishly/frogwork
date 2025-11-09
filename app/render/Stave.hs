module Stave where

import Data.Bifunctor (second)

import Foreign (peek, peekArray, withArray)

import FreeType.Core.Base
import FreeType.Core.Types

import Graphics.Rendering.OpenGL (
    Clamping (..)
  , PixelFormat (..)
  , Repetition (..)
  , TextureCoordName (..)
  , TextureFilter (..)
  , TextureObject
  , TextureTarget2D (..)
  , ($=)
  )
import qualified Graphics.Rendering.OpenGL as GL (
    textureFilter
  , textureWrapMode
  )

import Mean (twimap, twin, doBoth)
import Shade (helpMe)


glyphFormatName :: FT_Glyph_Format -> String
glyphFormatName format = "ft_GLYPH_FORMAT_" ++ case format of
    FT_GLYPH_FORMAT_BITMAP -> "BITMAP"
    FT_GLYPH_FORMAT_COMPOSITE -> "COMPOSITE"
    FT_GLYPH_FORMAT_OUTLINE -> "OUTLINE"
    FT_GLYPH_FORMAT_PLOTTER -> "PLOTTER"
    _ -> "NONE"

pad :: Int -> Int -> a -> [a] -> [a]
pad _ _ _ [] = []
pad amount width something bitmapData = aLeft ++ b ++ c where
  (aLeft, aRight) = splitAt width bitmapData
  b = replicate amount something
  c = pad amount width something aRight

-- | This is the test function to see if @loadStave@
-- successfully begets a @TextureObject@.
loadGlyphWithFile :: IO TextureObject
loadGlyphWithFile = loadStave "assets/noto-sans.ttf" 'o' 24

-- | Based on [this page](https://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html).
loadStave :: FilePath -> Char -> Int -> IO TextureObject
loadStave path stave greatness = do
  stavebook <- ft_Init_FreeType
  putStrLn "made stavebook!"

  feather <- ft_New_Face stavebook path 0
  ft_Set_Pixel_Sizes feather 0 (fromIntegral greatness)
  feather' <- peek feather
  putStrLn "made feather!"
  -- mapM [61..
  finger <- ft_Get_Char_Index feather (fromIntegral $ fromEnum stave)
  ft_Load_Glyph feather finger FT_LOAD_RENDER
  putStrLn "made finger!"
  putStrLn $ "finger is: " ++ show finger

  let slot = frGlyph feather'
  slot' <- peek slot
  let staveTell = frNum_glyphs feather'
  let shape = gsrFormat slot'
  putStrLn "made slot!"
  putStrLn $ "slot is: " ++ show slot
  putStrLn $ "stave tell is: " ++ show staveTell
  putStrLn $ "stave shape is: " ++ glyphFormatName shape

  -- this is segfaulting, but is just for logging purposes so it can be ignored.
  -- let greatnessTell = frNum_fixed_sizes feather'
  --     sizesPtr = frAvailable_sizes feather'
  -- _sizes <- forM [0 .. (fromIntegral greatnessTell)]
  --   (\i -> peek . plusPtr sizesPtr . fromIntegral $ i :: IO FT_Bitmap_Size)
  -- putStrLn "greatnesses grabbed!"

  ft_Render_Glyph slot FT_RENDER_MODE_NORMAL

  let bitmap = gsrBitmap slot'
      (l, t) = doBoth gsrBitmap_left gsrBitmap_top slot'
  putStrLn "here's what the bitmap looks like:"
  putStrLn $ "  lefttop: " ++ show (l, t)
  putStrLn $ "  width: " ++ show (bWidth bitmap)
  putStrLn $ "  rows: " ++ show (bRows bitmap)
  putStrLn $ "  pitch: " ++ show (bPitch bitmap)
  putStrLn $ "  num_grays: " ++ show (bNum_grays bitmap)
  putStrLn $ "  pixel_mode: " ++ show (bPixel_mode bitmap)
  putStrLn $ "  palette_mode: " ++ show (bPalette_mode bitmap)
  putStrLn ""

  let ((w, w'), (h, h')) = twimap (second fromIntegral . twin . fromIntegral . ($ bitmap)) (bWidth, bRows)
      pitch = 4 - mod w 4
      nw = fromIntegral (pitch + fromIntegral w')
  putStrLn "did some reckoning..."

  tex <- flip withArray (helpMe Red (nw, h')) . pad pitch w 0 =<< peekArray (w*h) (bBuffer bitmap)

  -- GL.texture Texture2D $= Enabled -- this doesnt seem to do anything
  GL.textureFilter Texture2D $= ((Linear', Nothing), Linear')
  GL.textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  GL.textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  putStrLn "made texture!"

  ft_Done_Face feather
  ft_Done_FreeType stavebook

  return tex
