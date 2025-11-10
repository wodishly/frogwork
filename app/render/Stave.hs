{- HLINT ignore "Redundant return" -}
module Stave (
  Stave (..)
, Stavebook
, loadFeather
, loadStaveweb
, stavewrite
) where

import Control.Monad (forM, forM_)
import Data.Char (chr)
import Data.HashMap.Lazy (HashMap, fromList, (!))
import Text.Printf (printf)

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
  , Vertex2 (Vertex2)
  , GLfloat
  , ($=), activeTexture, TextureUnit (TextureUnit), textureBinding, bindBuffer, BufferTarget (ArrayBuffer), bufferSubData, TransferDirection (WriteToBuffer)
  )
import qualified Graphics.Rendering.OpenGL as GL (
    textureFilter
  , textureWrapMode
  )

import FastenMain (assetsBasePath)
import Matrix (Point)
import Mean (doBoth, ly, twimap, (.>>.))
import Shade (helpMe, useMesh, Mesh (..), bufferSize, drawFaces)
import FastenShade (floorVBuffer)

type Stavebook = HashMap Char Stave

data Stave = Stave {
    bearing :: Point
  , size :: Point
  , advance :: GLfloat
  , texture :: TextureObject
} deriving (Show, Eq)

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

loadFeather :: FilePath -> IO Stavebook
loadFeather = flip loadStaveweb 24 . printf "%s/%s.ttf" assetsBasePath

-- | Based on [this page](https://zyghost.com/articles/Haskell-font-rendering-with-freetype2-and-opengl.html).
loadStaveweb :: FilePath -> FT_UInt -> IO Stavebook
loadStaveweb path greatness = do
  stavewit <- ft_Init_FreeType
  putStrLn "made stavebook!"

  feather <- ft_New_Face stavewit path 0
  ft_Set_Pixel_Sizes feather 0 (fromIntegral greatness)
  feather' <- peek feather
  putStrLn "made feather!"

  stavebook <- forM (map chr [65..90]) $ \stave -> do
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

    -- this is segfaulting, but is just for logging purposes so it can be ignored.
    -- let greatnessTell = frNum_fixed_sizes feather'
    --     sizesPtr = frAvailable_sizes feather'
    -- _sizes <- forM [0 .. (fromIntegral greatnessTell)]
    --   (\i -> peek . plusPtr sizesPtr . fromIntegral $ i :: IO FT_Bitmap_Size)
    -- putStrLn "greatnesses grabbed!"

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

    let (w', h') = twimap fromIntegral (w, h)
        pitch = 4 - mod w' 4
        nw = fromIntegral (pitch + fromIntegral w')
    putStrLn "did some reckoning..."

    tex' <- flip withArray (helpMe Red (nw, fromIntegral h')) . pad pitch w' 0 =<< peekArray (w'*h') (bBuffer bitmap)

    -- GL.texture Texture2D $= Enabled -- this doesnt seem to do anything
    GL.textureFilter Texture2D $= ((Linear', Nothing), Linear')
    GL.textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    GL.textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
    putStrLn "made texture!"

    return $ ly (stave, Stave
        (fromIntegral <$> Vertex2 l t)
        (fromIntegral <$> Vertex2 w h)
        (fromIntegral (x .>>. 6))
        tex'
      )

  ft_Done_Face feather
  ft_Done_FreeType stavewit

  return $ fromList stavebook

stavewrite :: Stavebook -> Mesh -> Point -> GLfloat -> String -> IO ()
stavewrite book meshy (Vertex2 x y) scale spell = do

  useMesh meshy

  -- let advances = scanl (+) x (map (advance . (book!)) spell)

  -- forM_ (zip [0..] spell) $ \(i, stave) -> do
  --   let (Stave (Vertex2 left top) size'@(Vertex2 _ height) _ tex') = book!stave
  --       xpos = scale * (left + (advances!!i))
  --       ypos = y - scale * (height - top)
  --       Vertex2 w h = (* scale) <$> size'
  --       -- vertices = [
  --       --     xpos+w, ypos  , 0
  --       --   , xpos+w, ypos+h, 0
  --       --   , xpos  , ypos+h, 0
  --       --   , xpos  , ypos  , 0
  --       --   ]
  --       vertices = floorVBuffer

  --   activeTexture $= TextureUnit 0
    -- textureBinding Texture2D $= Just tex'
    -- bindBuffer ArrayBuffer $= Just (vbo meshy)

    -- _ <- withArray vertices $ \ptr ->
    --   bufferSubData ArrayBuffer WriteToBuffer 0 (bufferSize vertices) ptr
    
    -- bindBuffer ArrayBuffer $= Nothing
    -- drawFaces $ elementCount meshy