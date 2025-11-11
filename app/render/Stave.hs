module Stave (
  Stave (..)
, Stavebook
, Staveware
, makeFeather
, makeStavebook
, stavewrite
) where

import Control.Monad (forM, forM_)
import Data.Char (chr)
import Data.HashMap.Lazy (HashMap, fromList, (!))
import Text.Printf (printf)

import Foreign (Word32, peek, peekArray, withArray)

import FreeType.Core.Base
import FreeType.Core.Types

import Graphics.Rendering.OpenGL (
    BufferTarget (ArrayBuffer)
  , Clamping (ClampToEdge)
  , GLfloat
  , PixelFormat (Red)
  , Repetition (Repeated)
  , TextureCoordName (S, T)
  , TextureFilter (Linear')
  , TextureObject
  , TextureTarget2D (Texture2D)
  , TextureUnit (TextureUnit)
  , TransferDirection (WriteToBuffer)
  , Vertex2 (Vertex2)
  , Vertex3 (Vertex3)
  , activeTexture
  , bindBuffer
  , bufferSubData
  , textureBinding
  , ($=)
  )
import qualified Graphics.Rendering.OpenGL as GL (
    textureFilter
  , textureWrapMode
  )

import FastenMain (assetsBasePath)

import Matrix (Point, Polyhedron, (<+>), (*^))
import Mean (doBoth, (.>>.))
import Shade (uploadTexture, useMesh, Mesh (..), bufferSize, drawFaces)

type Stavebook = HashMap Char Stave
type Staveware = (Stavebook, Mesh)

data Stave = Stave {
    bearing :: Point -- top left
  , size :: Point
  , advance :: GLfloat -- step
  , texture :: TextureObject
} deriving (Show, Eq)

sharpness :: Word32
sharpness = 2^(7 :: Integer)

greatness :: GLfloat
greatness = 2^(6 :: Integer)

glyphFormatName :: FT_Glyph_Format -> String
glyphFormatName = ("ft_GLYPH_FORMAT_" ++) . \case
    FT_GLYPH_FORMAT_BITMAP -> "BITMAP"
    FT_GLYPH_FORMAT_COMPOSITE -> "COMPOSITE"
    FT_GLYPH_FORMAT_OUTLINE -> "OUTLINE"
    FT_GLYPH_FORMAT_PLOTTER -> "PLOTTER"
    _ -> "NONE"

pad :: Int -> Int -> a -> [a] -> [a]
pad _ _ _ [] = []
pad amount width something bitmapData = left ++ b ++ recourse where
  (left, right) = splitAt width bitmapData
  b = replicate amount something
  recourse = pad amount width something right

makeFeather :: FilePath -> IO Stavebook
makeFeather = makeStavebook sharpness . printf "%s/%s.ttf" assetsBasePath

-- | Based on [this page](https://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html).
makeStavebook :: FT_UInt -> FilePath -> IO Stavebook
makeStavebook great path = do
  stavewit <- ft_Init_FreeType
  putStrLn "made stavebook!"

  feather <- ft_New_Face stavewit path 0
  ft_Set_Pixel_Sizes feather 0 (fromIntegral great)
  feather' <- peek feather
  putStrLn "made feather!"

  stavebook <- forM (map chr [32..126]) $ \stave -> do
    finger <- ft_Get_Char_Index feather (fromIntegral $ fromEnum stave)
    ft_Load_Glyph feather finger FT_LOAD_RENDER
    putStrLn "made finger!"
    putStrLn $ "finger is: " ++ show finger ++ " (" ++ show stave ++ ")"

    let slot = frGlyph feather'
    slot' <- peek slot
    let staveTell = frNum_glyphs feather'
    let shape = gsrFormat slot'
    putStrLn "made slot!"
    putStrLn $ "slot is: " ++ show slot
    putStrLn $ "stave tell is: " ++ show staveTell
    putStrLn $ "stave shape is: " ++ glyphFormatName shape

    ft_Render_Glyph slot FT_RENDER_MODE_NORMAL

    let bitmap = gsrBitmap slot'
        (l, t) = doBoth gsrBitmap_left gsrBitmap_top slot'
        (w, h) = doBoth bWidth bRows bitmap
        FT_Vector x _ = gsrAdvance slot'
    putStrLn "here's the stuff we're going to save"
    putStrLn $ "  bearing: " ++ show (l, t)
    putStrLn $ "  size: " ++ show (w, h)
    putStrLn $ "  advance: " ++ show (x, 0 :: Int)

    putStrLn "here's some other stuff:"
    putStrLn $ "  pitch: " ++ show (bPitch bitmap)
    putStrLn $ "  num_grays: " ++ show (bNum_grays bitmap)
    putStrLn $ "  pixel_mode: " ++ show (bPixel_mode bitmap)
    putStrLn $ "  palette_mode: " ++ show (bPalette_mode bitmap)
    putStrLn ""

    let (w', h') = (fromIntegral w, fromIntegral h)
        pitch = 4 - mod w' 4
        nw = fromIntegral (pitch + w')
    putStrLn "did some reckoning..."

    tex' <- flip withArray (uploadTexture Red (nw, h')) . pad pitch w' 0
      =<< peekArray (fromIntegral $ w*h) (bBuffer bitmap)

    -- does this do anything? unsure if safe to destroy
    -- GL.texture Texture2D $= Enabled
    GL.textureFilter Texture2D $= ((Linear', Nothing), Linear')
    GL.textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    GL.textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
    putStrLn "made texture!"

    return (stave, Stave
        (fromIntegral <$> Vertex2 l t)
        (fromIntegral <$> Vertex2 w h)
        (fromIntegral (x .>>. 6))
        tex'
      )

  ft_Done_Face feather
  ft_Done_FreeType stavewit

  return $ fromList stavebook

stavewrite :: Staveware -> Point -> GLfloat -> String -> IO ()
stavewrite (book, mesh) bottomLeft scale spell = do
  useMesh mesh

  let advances = scanl (+) 0 (map (advance . (book!)) spell)

  forM_ (zip [0..] spell) $ \(i, char) -> do
    let stave = book!char
        vertices = stavenooks stave scale bottomLeft (advances!!i)

    activeTexture $= TextureUnit 0
    textureBinding Texture2D $= Just (texture stave)
    bindBuffer ArrayBuffer $= Just (vbo mesh)

    withArray vertices (bufferSubData ArrayBuffer WriteToBuffer 0 $ bufferSize vertices)

    bindBuffer ArrayBuffer $= Nothing
    drawFaces $ elementCount mesh

stavenooks :: Stave -> GLfloat -> Point -> GLfloat -> Polyhedron
stavenooks stave scale bottomLeft step = [
    Vertex3 (x+w) (y+h) 0
  , Vertex3 (x+w)  y    0
  , Vertex3  x     y    0
  , Vertex3  x    (y+h) 0
  ] where
    (Stave (Vertex2 left top) z@(Vertex2 _ height) _ _) = stave
    scale' = scale * greatness / fromIntegral sharpness
    Vertex2 x y = bottomLeft <+> (scale' *^ Vertex2 (left + step) (top - height))
    Vertex2 w h = scale' *^ z
