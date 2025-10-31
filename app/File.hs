module File where

import Control.Lens.Internal.CTypes (Int16, Int32, Word32, Word8)
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Graphics.Rendering.OpenGL

getFrogBytes :: IO BL.ByteString
getFrogBytes = BL.readFile "assets/test.frog"

data FrogFile = FrogFile
  { vertexCount :: Int32,
    indexCount :: Int32,
    texWidth :: Int16,
    texHeight :: Int16,
    positionBuffer :: [Vertex3 GLfloat],
    uvBuffer :: [Vertex2 GLfloat],
    indexBuffer :: [Word32],
    bitmapBuffer :: [Word8]
  }
  deriving (Show)

data FrogVertex = FrogVertex
  { position :: Vertex3 GLfloat,
    uv :: Vertex2 GLfloat
  }
  deriving (Show)

parseFrogPosition :: Get (Vertex3 GLfloat)
parseFrogPosition = do
  x <- getFloatle
  y <- getFloatle
  z <- getFloatle
  return $! Vertex3 x y z

parseFrogUv :: Get (Vertex2 GLfloat)
parseFrogUv = do
  u <- getFloatle
  v <- getFloatle
  return $! Vertex2 u v

parseFrogFile :: Get FrogFile
parseFrogFile = do
  vcount <- getInt32le
  icount <- getInt32le
  twidth <- getInt16le
  theight <- getInt16le
  let tsize = fromIntegral twidth * fromIntegral theight * 4
  fverts <- mapM (const parseFrogPosition) [0 .. (vcount - 1)]
  fuvs <- mapM (const parseFrogUv) [0 .. (vcount - 1)]
  findices <- mapM (const getWord32le) [0 .. (icount - 1)]
  bmp <- mapM (const getWord8) [(0 :: Integer) .. (tsize - 1)]
  return $!
    FrogFile
      vcount
      icount
      twidth
      theight
      fverts
      fuvs
      findices
      bmp
