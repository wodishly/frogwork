module FrogSpell (
  parseFrogFile
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

import Data.Binary.Get (Get, getFloatle, getInt16le, getInt32le, getWord32le, getWord8)
import Graphics.Rendering.OpenGL

import Foreign (Int16, Int32, Word32, Word8)
import Matrix (Polygon, Polyhedron)
import Control.Lens (makeLenses)
import Spell ( f32x3, (✿), int )

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

f32x2 :: Get (Vertex2 GLfloat)
f32x2 = do
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

  fverts <- vcount ✿ f32x3
  fuvs <- vcount ✿ f32x2
  fnormals <- ncount ✿ f32x3
  findices <- icount ✿ getWord32le

  let tsize = int twidth * int theight * 4
  bmp <- tsize ✿ getWord8

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
