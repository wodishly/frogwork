module File where

import Control.Lens.Internal.CTypes (Int16, Int32, Word32, Word8)
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Graphics.Rendering.OpenGL
import Control.Monad

getFrogBytes :: String -> IO BL.ByteString
getFrogBytes = BL.readFile

data FrogFile = FrogFile { 
  -- header
  vertexCount :: Int32
, indexCount :: Int32
, texWidth :: Int16
, texHeight :: Int16
  -- vertex attributes
, positionBuffer :: [Vertex3 GLfloat]
, uvBuffer :: [Vertex2 GLfloat]
  -- face indices
, indexBuffer :: [Word32]
  -- rgba texture block
, bitmapBuffer :: [Word8]
} deriving (Show)

data FrogVertex = FrogVertex {
  position :: Vertex3 GLfloat
, uv :: Vertex2 GLfloat
} deriving (Show)

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
  fverts <- replicateM (fromIntegral vcount) parseFrogPosition
  fuvs <- replicateM (fromIntegral vcount) parseFrogUv
  findices <- replicateM (fromIntegral icount) getWord32le
  bmp <- replicateM tsize getWord8

  return $! FrogFile
    vcount
    icount
    twidth
    theight
    fverts
    fuvs
    findices
    bmp
