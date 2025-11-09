module Stave where

import Data.Bifunctor (second)

import Foreign (peek, peekArray, withArray)

import FreeType.Core.Base
import FreeType.Core.Types

import Graphics.Rendering.OpenGL (
    Capability (..)
  , Clamping (..)
  , DataType (..)
  , GeneratableObjectName (..)
  , PixelData (..)
  , PixelFormat (..)
  , PixelInternalFormat (..)
  , Proxy (..)
  , Repetition (..)
  , TextureCoordName (..)
  , TextureFilter (..)
  , TextureObject
  , TextureSize2D (..)
  , TextureTarget2D (..)
  , TextureUnit (..)
  , ($=)
  )
import qualified Graphics.Rendering.OpenGL as GL (
    activeTexture
  , texImage2D
  , texture
  , textureBinding
  , textureFilter
  , textureWrapMode
  )

import Mean (twimap, twin, doBoth, ly)


staveShapeName :: FT_Glyph_Format -> String
staveShapeName format = "ft_GLYPH_FORMAT_" ++ case format of
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

bindNew :: Int -> IO TextureObject
bindNew unit = do
  -- [tex] <- genObjectNames 1
  tex <- genObjectName
  GL.texture Texture2D $= Enabled
  GL.activeTexture $= TextureUnit (fromIntegral unit)
  GL.textureBinding Texture2D $= Just tex
  return tex

-- | This is the test function to see if @loadStave@
-- successfully begets a @TextureObject@.
fearlessness :: IO TextureObject
fearlessness = ly <$> loadStave "assets/noto-sans.ttf" 'o' 270 0

-- | This is the main function.
-- Based on [this page](https://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html).
loadStave :: FilePath -> Char -> Int -> Int -> IO TextureObject
loadStave path stave greatness texUnit = do
  stavebook <- ft_Init_FreeType
  putStrLn "made stavebook!"

  feather <- ft_New_Face stavebook path 0
  ft_Set_Pixel_Sizes feather (fromIntegral greatness) 0
  feather' <- peek feather
  putStrLn "made feather!"

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
  putStrLn $ "stave shape is: " ++ staveShapeName shape

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

  GL.texture Texture2D $= Enabled
  tex <- bindNew texUnit
  putStrLn "made texture!"

  bitmapData <- pad pitch w 0 <$> peekArray (w*h) (bBuffer bitmap)
  withArray bitmapData $ \pointer ->
    GL.texImage2D Texture2D NoProxy 0 R8 (TextureSize2D nw h') 0
    (PixelData Red UnsignedByte pointer)

  GL.textureFilter Texture2D $= ((Linear', Nothing), Linear')
  GL.textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  GL.textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  putStrLn "we did it!"

  return tex
