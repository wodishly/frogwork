module FrogSpell where

import Data.Binary.Get
import Graphics.Rendering.OpenGL
import Foreign

import Rime
import Spell


data FrogFile = FrogFile {
  -- header
  vertexCount :: Int32,
  normalCount :: Int32,
  indexCount :: Int32,
  boneInfluence :: Int32, -- 0 or 4, but int32 for alignment
  texSize :: (Int16, Int16),
  -- vertex attributes
  positionBuffer :: Polyhedron,
  uvBuffer :: Polygon,
  normalBuffer :: Polyhedron,
  boneBuffer :: ZPolytope,
  weightBuffer :: Polytope,
  -- face indices
  indexBuffer :: [Word32],
  -- rgba texture block
  bitmapBuffer :: [Word8]
} deriving (Show, Eq)

data FrogVertex = FrogVertex {
  position :: Vertex3 GLfloat
, uv :: Vertex3 GLfloat
, normal :: Vertex3 GLfloat
} deriving (Show, Eq)

frogify :: Get FrogFile
frogify = do
  vcount <- s32
  ncount <- s32
  icount <- s32
  bcount <- s32
  twidth <- s16
  theight <- s16

  -- basic attrs
  fverts <- vcount ✿ f32x3
  fuvs <- vcount ✿ f32x2
  fnormals <- ncount ✿ f32x3

  -- skeletal attrs
  let bonemany = if bcount /= 4 then 0 else vcount
  fbones <- bonemany ✿ u8x4
  fweights <- bonemany ✿ f32x4

  -- faces
  findices <- icount ✿ u32

  -- texture
  let tsize = int twidth * int theight * 4
  bmp <- tsize ✿ u8

  return $!
    FrogFile {
        vertexCount = vcount
      , normalCount = ncount
      , indexCount = icount
      , boneInfluence = bcount
      , texSize = (twidth, theight)
      , positionBuffer = fverts
      , uvBuffer = fuvs
      , normalBuffer = fnormals
      , boneBuffer = fbones
      , weightBuffer = fweights
      , indexBuffer = findices
      , bitmapBuffer = bmp
    }
