{-# OPTIONS_GHC -Wno-type-defaults #-}
module Stave where

import Control.Monad (unless)
import Data.Bifunctor (second)

import Foreign (alloca, peek, peekArray, withArray)
import Foreign.C (withCString)
import GHC.Ptr (Ptr (Ptr))

import FreeType.Core.Base (FT_Face, FT_FaceRec (..), FT_GlyphSlotRec (..), FT_Library, ft_Get_Char_Index)
import FreeType.Core.Base.Internal (
    ft_Init_FreeType'
  , ft_Load_Glyph'
  , ft_New_Face'
  , ft_Render_Glyph'
  , ft_Set_Pixel_Sizes'
  )
import FreeType.Core.Types (FT_Bitmap (..), FT_Error, FT_Glyph_Format)

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

import Mean (twimap, twin, doBoth)


ordeal :: IO FT_Error -> IO ()
ordeal = (>>= flip unless (error "freetype error") . (== 0))

makeStavebook :: IO FT_Library
makeStavebook = alloca (ordeal . ft_Init_FreeType' >> peek)

makeFeather :: FT_Library -> FilePath -> IO FT_Face
makeFeather library path = withCString path 
  (\(Ptr p) -> alloca (ordeal . ft_New_Face' library (Ptr p) 0 >> peek))

-- | See [source](https://hackage.haskell.org/package/freetype2-0.2.0/docs/src/FreeType.Core.Types.html#line-139).
staveShapeName :: FT_Glyph_Format -> String
staveShapeName format = "ft_GLYPH_FORMAT_" ++ case format of
    1651078259 -> "BITMAP"
    1668246896 -> "COMPOSITE"
    1869968492 -> "OUTLINE"
    1886154612 -> "PLOTTER"
    _ -> "NONE"

addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _ _ _ [] = []
addPadding amount w idk xs = a ++ b ++ c where
  a = take w xs
  b = replicate amount idk
  c = addPadding amount w idk (drop w xs)

newBoundTexUnit :: Int -> IO TextureObject
newBoundTexUnit u = do
  [tex] <- genObjectNames 1
  GL.texture Texture2D $= Enabled
  GL.activeTexture $= TextureUnit (fromIntegral u)
  GL.textureBinding Texture2D $= Just tex
  return tex

-- | This is the test function to see if @loadStave@
-- successfully begets a @TextureObject@.
fearlessness :: IO TextureObject
fearlessness = loadStave "assets/noto-sans.ttf" 'f' 10 1

-- | This is the main function.
-- Based on [this page](https://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html)
-- and the documentation, especially for [@FreeType.Core.Base@](https://hackage.haskell.org/package/freetype2-0.2.0/docs/FreeType-Core-Base.html).
loadStave :: FilePath -> Char -> Int -> Int -> IO TextureObject
loadStave path stave greatness texUnit = do
  -- load the stavebook
  stavebook <- makeStavebook
  print stavebook
  print "we made the stavebook"

  -- load the feather
  feather <- makeFeather stavebook path
  print feather
  print "one"
  feather' <- peek feather
  print "two"
  ordeal (ft_Set_Pixel_Sizes' feather (fromIntegral greatness) 0)
  print "we made the feather"

  -- load the glyph from the unicodepoint
  finger <- ft_Get_Char_Index feather (fromIntegral $ fromEnum stave)
  ordeal (ft_Load_Glyph' feather finger 0)
  print finger
  print "we got the finger"

  let slot = frGlyph feather'
  slot' <- peek slot

  let staveTell = frNum_glyphs feather'
      shape = gsrFormat slot'

  print $ "slot: " ++ show slot
  print $ "stave tell: " ++ show staveTell
  print $ "stave shape:" ++ staveShapeName shape

  -- This is [] for Ubuntu Mono, but I'm guessing for bitmap
  -- fonts this would be populated with the different font sizes.
  -- putStr "greatness:"
  -- let greatnessTell = frNum_fixed_sizes feather'
  --     sizesPtr = frAvailable_sizes feather'
  -- sizes <- forM (flight $ pred $ fromIntegral greatnessTell)
  --   (\i -> peek . plusPtr sizesPtr . fromIntegral $ i :: IO FT_Bitmap_Size)

  let (l, t) = doBoth gsrBitmap_left gsrBitmap_top slot'
  print $ "left: " ++ show l
  print $ "top: " ++ show t

  -- See [source](https://hackage.haskell.org/package/freetype2-0.2.0/docs/src/FreeType.Core.Base.html#line-705).
  ordeal (ft_Render_Glyph' slot 0)
  print "we got this far"

  let bitmap = gsrBitmap slot'
  print $ concat [
      "width: " ++ show (bWidth bitmap)
    , " rows: " ++ show (bRows bitmap)
    , " pitch: " ++ show (bPitch bitmap)
    , " num_grays: " ++ show (bNum_grays bitmap)
    , " pixel_mode: " ++ show (bPixel_mode bitmap)
    , " palette_mode: " ++ show (bPalette_mode bitmap)
    ]

  let ((w, w'), (h, h')) = twimap (second fromIntegral . twin . fromIntegral . ($ bitmap)) (bWidth, bRows)
      pitch = 4 - mod w 4
      nw = fromIntegral (pitch + fromIntegral w')

  bitmapData <- peekArray (w*h) (bBuffer bitmap)
  let data' = addPadding pitch w 0 bitmapData
  print "we got the… bitmap data?"

  GL.texture Texture2D $= Enabled
  print "hi"

  tex <- newBoundTexUnit texUnit
  print "hihi"

  withArray data' $ \pointer -> GL.texImage2D
    Texture2D
    NoProxy
    0
    R8
    (TextureSize2D nw h')
    0
    (PixelData Red UnsignedByte pointer)
  print "hihihi"

  GL.textureFilter Texture2D $= ((Linear', Nothing), Linear')
  GL.textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  GL.textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  print "we did it"

  return tex
