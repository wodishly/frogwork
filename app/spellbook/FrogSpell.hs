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

import Data.Binary.Get (Get)
import Graphics.Rendering.OpenGL

import Foreign (Int16, Int32, Word32, Word8)
import Matrix (Polygon, Polyhedron)
import Control.Lens (makeLenses)
import Spell ((✿), int, u8, u32, s16, s32, f32x2, f32x3)

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

parseFrogFile :: Get FrogFile
parseFrogFile = do
  vcount <- s32
  ncount <- s32
  icount <- s32
  twidth <- s16
  theight <- s16

  fverts <- vcount ✿ f32x3
  fuvs <- vcount ✿ f32x2
  fnormals <- ncount ✿ f32x3
  findices <- icount ✿ u32

  let tsize = int twidth * int theight * 4
  bmp <- tsize ✿ u8

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
