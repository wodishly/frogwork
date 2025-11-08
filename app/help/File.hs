module File (
  getFrogBytes
, parseFrogFile
, FrogFile (..)
, FrogVertex (..)
) where

import Control.Monad (replicateM)
import Data.Binary.Get (Get, getFloatle, getInt16le, getInt32le, getWord32le, getWord8)

import Foreign (Int16, Int32, Word32, Word8)
import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3))

import qualified Data.ByteString.Lazy as BL

import Matrix (Polygon, Polyhedron)


getFrogBytes :: String -> IO BL.ByteString
getFrogBytes = BL.readFile

data FrogFile = FrogFile {
  -- header
  vertexCount :: Int32,
  normalCount :: Int32,
  indexCount :: Int32,
  texWidth :: Int16,
  texHeight :: Int16,
  -- vertex attributes
  positionBuffer :: Polyhedron,
  uvBuffer :: Polygon,
  normalBuffer :: Polyhedron,
  -- face indices
  indexBuffer :: [Word32],
  -- rgba texture block
  bitmapBuffer :: [Word8]
} deriving (Show, Eq)

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
    FrogFile
      vcount
      ncount
      icount
      twidth
      theight
      fverts
      fuvs
      fnormals
      findices
      bmp
