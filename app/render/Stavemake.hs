{-|
@Stavemake@ is for anything to do with begetting the stave.
@State@ imports @Stavemake@, so @Stavemake@ must not import @State@.
-}
module Stavemake (
  Stave (..),
  Stavebook,
  Staveware,
  makeFeather,
  makeStavebook,
  makeStavebook', -- uncalled
) where

import Control.Monad (forM, when)
import Data.Char (chr)
import Data.HashMap.Lazy (HashMap, fromList)
import Data.List.Split (chunksOf)
import Text.Printf (printf)

import Foreign (peek, peekArray, withArray)
import FreeType.Core.Base
import FreeType.Core.Types
import Graphics.Rendering.OpenGL (
  Clamping (ClampToEdge),
  GLfloat,
  PixelFormat (Red),
  Repetition (Repeated),
  TextureCoordName (S, T),
  TextureFilter (Linear'),
  TextureObject,
  TextureTarget2D (Texture2D),
  Vertex2 (Vertex2),
  ($=),
  )
import qualified Graphics.Rendering.OpenGL as GL (
  textureFilter,
  textureWrapMode
  )

import FastenMain (wayToFeathers, orheight, staveSharpness)

import Mean (doBoth, (.>>.))
import Rime (Point)
import Shade (Mesh (..), uploadTexture)


data Stave = Stave {
  bearing :: Point, -- top left
  size :: Point,
  advance :: GLfloat, -- step
  texture :: TextureObject
} deriving (Show, Eq)

type Stavebook = HashMap Char Stave
type Staveware = (Stavebook, Mesh)

tokenwit :: [Int]
tokenwit =
     [32..126] -- mean
  ++ [160..255] -- full-1
  ++ [256..383] -- thin-a
  ++ [384..591] -- thin-b
  ++ [592..687] -- loud
  ++ [0x2c60..0x2c7f] -- thin-c
  ++ [0x0370..0x03ff] -- ellen, head

  -- greater loudtokens
  ++ [0x2c6d, -- Ɑ
      0x2c70, -- Ɒ
      0x2c6f, -- Ɐ
      0x0181, -- Ɓ
      0xa7b4, -- Ꞵ
      0x00d0, -- Ð
      0x018a, -- Ɗ
      0x018f, -- Ə
      0x0190, -- Ɛ
      0xa7ab, -- Ɜ
      0x0193, -- Ɠ
      0x0194, -- Ɣ
      0x0126, -- Ħ
      0xa7aa, -- Ɦ
      0xa78d, -- Ɥ
      0x0197, -- Ɨ
      0xa7ae, -- Ɪ
      0xa7b2, -- Ʝ
      0x2c62, -- Ɫ
      0xa7ad, -- Ɬ
      0x2c6e, -- Ɱ
      0x014a, -- Ŋ
      0x019d, -- Ɲ
      0x0186, -- Ɔ
      0x019f, -- Ɵ
      0x2c64, -- Ɽ
      0xa7c5, -- Ʂ
      0x01a9, -- Ʃ
      0x01ae, -- Ʈ
      0x0244, -- Ʉ
      0x01b1, -- Ʊ
      0x01b2, -- Ʋ
      0x0245, -- Ʌ
      0x019c, -- Ɯ
      0x01b7, -- Ʒ
      0x0241  -- Ɂ
     ]

glyphFormatName :: FT_Glyph_Format -> String
glyphFormatName = ("ft_GLYPH_FORMAT_" ++) . \case
    FT_GLYPH_FORMAT_BITMAP -> "BITMAP"
    FT_GLYPH_FORMAT_COMPOSITE -> "COMPOSITE"
    FT_GLYPH_FORMAT_OUTLINE -> "OUTLINE"
    FT_GLYPH_FORMAT_PLOTTER -> "PLOTTER"
    _ -> "NONE"

pad :: Num a => Int -> Int -> [a] -> [a]
pad _ _ [] = []
pad gap width bitmap = concatMap (++ replicate gap 0) (chunksOf width bitmap)

makeFeather :: FilePath -> IO Stavebook
makeFeather = makeStavebook staveSharpness . printf "%s/%s.ttf" wayToFeathers

-- | does it softly
makeStavebook :: FT_UInt -> FilePath -> IO Stavebook
makeStavebook = makeStavebook'' False

-- | does it loudly
makeStavebook' :: FT_UInt -> FilePath -> IO Stavebook
makeStavebook' = makeStavebook'' True

-- | Based on [this page](https://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html).
makeStavebook'' :: Bool -> FT_UInt -> FilePath -> IO Stavebook
makeStavebook'' loud sharp road = do

  stavewit <- ft_Init_FreeType
  when loud $ putStrLn "made stavebook!"

  feather <- ft_New_Face stavewit road 0

  ft_Set_Char_Size feather 0 (8 * fromIntegral sharp) 0 orheight
  feather' <- peek feather
  when loud $ putStrLn "made feather!"

  stavebook <- forM (map chr tokenwit) $ \stave -> do

    finger <- ft_Get_Char_Index feather (fromIntegral $ fromEnum stave)
    ft_Load_Glyph feather finger FT_LOAD_RENDER
    when loud $ putStrLn "made finger!"
    when loud $ putStrLn $ "finger is: " ++ show finger ++ " (" ++ show stave ++ ")"

    let slot = frGlyph feather'
    slot' <- peek slot

    let staveTell = frNum_glyphs feather'
        shape = gsrFormat slot'
    when loud $ putStrLn "made slot!"
    when loud $ putStrLn $ "slot is: " ++ show slot
    when loud $ putStrLn $ "stave tell is: " ++ show staveTell
    when loud $ putStrLn $ "stave shape is: " ++ glyphFormatName shape

    ft_Render_Glyph slot FT_RENDER_MODE_NORMAL

    let bitmap = gsrBitmap slot'
        (l, t) = doBoth gsrBitmap_left gsrBitmap_top slot'
        (w, h) = doBoth bWidth bRows bitmap
        FT_Vector x _ = gsrAdvance slot'
    when loud $ putStrLn "here's the stuff we're going to save"
    when loud $ putStrLn $ "  bearing: " ++ show (l, t)
    when loud $ putStrLn $ "  size: " ++ show (w, h)
    when loud $ putStrLn $ "  advance: " ++ show (x, 0)

    when loud $ putStrLn "here's some other stuff:"
    when loud $ putStrLn $ "  pitch: " ++ show (bPitch bitmap)
    when loud $ putStrLn $ "  num_grays: " ++ show (bNum_grays bitmap)
    when loud $ putStrLn $ "  pixel_mode: " ++ show (bPixel_mode bitmap)
    when loud $ putStrLn $ "  palette_mode: " ++ show (bPalette_mode bitmap)
    when loud $ putStrLn ""

    let (w', h') = (fromIntegral w, fromIntegral h)
        gap = 4 - mod w' 4
        nw = fromIntegral (gap + w')

    when loud $ putStrLn "did some reckoning..."

    tex' <- flip withArray (uploadTexture Red (nw, h')) . pad gap w'
      =<< peekArray (fromIntegral $ w*h) (bBuffer bitmap)

    -- does this do anything? unsure if safe to destroy
    -- GL.texture Texture2D $= Enabled

    GL.textureFilter Texture2D $= ((Linear', Nothing), Linear')
    GL.textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    GL.textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
    when loud $ putStrLn "made texture!"

    return (stave, Stave
        (fromIntegral <$> Vertex2 l t)
        (fromIntegral <$> Vertex2 w h)
        (fromIntegral $ x .>>. 6)
        tex'
      )

  ft_Done_Face feather
  ft_Done_FreeType stavewit

  return $ fromList stavebook
