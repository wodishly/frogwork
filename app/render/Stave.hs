module Stave where

import Control.Monad (unless)

import Foreign (alloca, peek, withArray, peekArray)
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

import Mean (twimap, twin, doBoth, ly)
import Data.Bifunctor (second)


ordeal :: IO FT_Error -> IO ()
ordeal = (>>= flip unless (error "freetype error") . (== 0))

makeStavebook :: IO FT_Library
makeStavebook = alloca $ \pointer -> do
  putStrLn "making stavebook..."
  ordeal (ft_Init_FreeType' pointer)
  putStrLn "stavebook withstood the ordeal `init_freetype`!"
  p <- peek pointer
  putStrLn $ "stavebook points to: " ++ show p
  putStrLn ""
  return p

makeFeather :: FT_Library -> FilePath -> IO FT_Face
makeFeather library path = withCString path $ \cstring@(Ptr char) -> alloca $ \pointer -> do
  putStrLn $ "making feather from " ++ show cstring ++ "..."
  ordeal (ft_New_Face' library (Ptr char) 0 pointer)
  putStrLn "feather withstood the ordeal `new_face`!"
  p <- peek pointer
  putStrLn $ "feather points to: " ++ show p
  putStrLn ""
  return p

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
  -- load the stavebook
  stavebook <- makeStavebook

  -- load the feather
  feather <- makeFeather stavebook path
  ordeal (ft_Set_Pixel_Sizes' feather (fromIntegral greatness) 0)
  putStrLn "feather withstood the ordeal `set_pixel_sizes`!"
  feather' <- peek feather
  putStrLn "feather record peeked!"
  putStrLn ""

  -- load the glyph from the unicodepoint
  finger <- ft_Get_Char_Index feather (fromIntegral $ fromEnum stave)
  ordeal (ft_Load_Glyph' feather finger 0)
  putStrLn "finger withstood the ordeal `load_glyph`!"
  putStrLn $ "finger is: " ++ show finger
  putStrLn ""

  let slot = frGlyph feather'
  putStrLn $ "slot is: " ++ show slot
  slot' <- peek slot
  putStrLn "slot record peeked!"
  putStrLn ""

  let staveTell = frNum_glyphs feather'
  putStrLn $ "stave tell is: " ++ show staveTell
  let shape = gsrFormat slot'
  putStrLn $ "stave shape is: " ++ staveShapeName shape
  putStrLn ""

  -- this is segfaulting
  -- let greatnessTell = frNum_fixed_sizes feather'
  --     sizesPtr = frAvailable_sizes feather'
  -- _sizes <- forM [0 .. (fromIntegral greatnessTell)]
  --   (\i -> peek . plusPtr sizesPtr . fromIntegral $ i :: IO FT_Bitmap_Size)
  -- putStrLn "greatnesses grabbed!"

  -- See [source](https://hackage.haskell.org/package/freetype2-0.2.0/docs/src/FreeType.Core.Base.html#line-705).
  ordeal (ft_Render_Glyph' slot 0)
  putStrLn "slot withstood the ordeal `render_glyph`!"

  let bitmap = gsrBitmap slot'
      (l, t) = doBoth gsrBitmap_left gsrBitmap_top slot'
  putStrLn "heres what the bitmap looks like:"
  putStrLn $ "  lefttop: " ++ show (l, t)
  putStrLn $ "  width: " ++ show (bWidth bitmap)
  putStrLn $ "  rows: " ++ show (bRows bitmap)
  putStrLn $ "  pitch: " ++ show (bPitch bitmap)
  putStrLn $ "  num_grays: " ++ show (bNum_grays bitmap)
  putStrLn $ "  pixel_mode: " ++ show (bPixel_mode bitmap)
  putStrLn $ "  palette_mode: " ++ show (bPalette_mode bitmap)
  putStrLn ""

  -- we need `second fromIntegral` if we're using `withArray` in the `texImage2D` call.
  let ((w, w'), (h, h')) = twimap (second fromIntegral . twin . fromIntegral . ($ bitmap)) (bWidth, bRows)
      pitch = 4 - mod w 4
      nw = fromIntegral (pitch + fromIntegral w')
  putStrLn "did some reckoning..."

  GL.texture Texture2D $= Enabled
  tex <- bindNew texUnit
  putStrLn "made texture!"

  -- notice that `bBuffer bitmap` is set at `nullPtr`
  -- i think this is what's causing the segfault.
  print $ bBuffer bitmap

  -- this segfaults, but is how things look [here](https://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html).
  -- bitmapData <- peekArray (w*h) $ bBuffer bitmap
  -- let bitmapData' = addPadding pitch w 0 bitmapData
  -- withArray bitmapData' $ \pointer ->
  --   GL.texImage2D Texture2D NoProxy 0 R8 (TextureSize2D nw h') 0
  --   (PixelData Red UnsignedByte pointer)

  -- this compiles, but renders goth, i.e. all black.
  GL.texImage2D Texture2D NoProxy 0 R8 (TextureSize2D nw h') 0
    (PixelData Red UnsignedByte $ bBuffer bitmap)

  GL.textureFilter Texture2D $= ((Linear', Nothing), Linear')
  GL.textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  GL.textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  putStrLn "we did it!"

  return tex
