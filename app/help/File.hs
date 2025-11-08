module File (
  getFrogBytes
, parseFrogFile
, FrogFile (..)
, FrogVertex (..)
, positionBuffer
, uvBuffer
, normalBuffer
, indexBuffer
, bitmapBuffer
, texSize
, indexCount
, normalCount -- unused
, vertexCount -- unused
) where

import Control.Monad (replicateM)
import Data.Binary.Get (Get, getFloatle, getInt16le, getInt32le, getWord32le, getWord8)

import Foreign (Int16, Int32, Word32, Word8)
import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3))

import qualified Data.ByteString.Lazy as BL

import Matrix (Polygon, Polyhedron)
import Control.Lens (makeLenses)


getFrogBytes :: String -> IO BL.ByteString
getFrogBytes = BL.readFile

data FrogFile = FrogFile {
  -- header
  _vertexCount :: Int32,
  _normalCount :: Int32,
  _indexCount :: Int32,
  _texSize :: (Int16, Int16),
  -- vertex attributes
  _positionBuffer :: Polyhedron,
  _uvBuffer :: Polygon,
  _normalBuffer :: Polyhedron,
  -- face indices
  _indexBuffer :: [Word32],
  -- rgba texture block
  _bitmapBuffer :: [Word8]
} deriving (Show, Eq)
makeLenses ''FrogFile

data FrogVertex = FrogVertex {
  position :: Vertex3 GLfloat
, uv :: Vertex2 GLfloat
, normal :: Vertex3 GLfloat
} deriving (Show, Eq)

parseFrogVector3 :: Get (Vertex3 GLfloat)
parseFrogVector3 = do
  x <- getFloatle
  y <- getFloatle
  z <- getFloatle
  return $! Vertex3 x y z

parseFrogVector2 :: Get (Vertex2 GLfloat)
parseFrogVector2 = do
  u <- getFloatle
  v <- getFloatle
  return $! Vertex2 u v

parseFrogFile :: Get FrogFile
parseFrogFile = do
  vcount <- getInt32le
  ncount <- getInt32le
  icount <- getInt32le
  twidth <- getInt16le
  theight <- getInt16le

  fverts <- replicateM (fromIntegral vcount) parseFrogVector3
  fuvs <- replicateM (fromIntegral vcount) parseFrogVector2
  fnormals <- replicateM (fromIntegral ncount) parseFrogVector3
  findices <- replicateM (fromIntegral icount) getWord32le

  let tsize = fromIntegral twidth * fromIntegral theight * 4
  bmp <- replicateM tsize getWord8

  return $!
    FrogFile {
        _vertexCount = vcount
      , _normalCount = ncount
      , _indexCount = icount
      , _texSize = (twidth, theight)
      , _positionBuffer = fverts
      , _uvBuffer = fuvs
      , _normalBuffer = fnormals
      , _indexBuffer = findices
      , _bitmapBuffer = bmp
    }
