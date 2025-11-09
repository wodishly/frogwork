module MothSpell (
  mothify
) where

import Control.Monad (replicateM)
import Data.Binary.Get (Get, getFloatle, getWord8)

import Foreign (Word8)
import Graphics.Rendering.OpenGL (GLfloat, Vertex4 (..))

import Matrix (FrogMatrix)
import Numeric.LinearAlgebra.HMatrix ((><))
import Spell ((✿), f32x3, int)
import Graphics.Rendering.OpenGL.GL (Vertex3)


data MothFile = MothFile {
  boneCount :: Word8,
  skeleton :: [MothBone],
  tracks :: [MothTale]
} deriving (Show, Eq)

data MothBone = MothBone {
  mother :: Word8,
  matrix :: FrogMatrix
} deriving (Show, Eq)

data MothTale = MothTale {
  position :: Mothly3,
  quaternion :: Mothly4,
  scale :: Mothly3
} deriving (Show, Eq)

data Mothly3 = Mothly3 {
  v3 :: [Vertex3 GLfloat],
  t3 :: [GLfloat]
} deriving (Show, Eq)

data Mothly4 = Mothly4 {
  v4 :: [Vertex4 GLfloat],
  t4 :: [GLfloat]
} deriving (Show, Eq)

exoskeleton :: Get MothBone
exoskeleton = do
  parent <- getWord8
  floats <- replicateM 16 getFloatle
  return $ MothBone {
    mother = parent
  , matrix = (4><4) floats
  }

tale :: Integer -> Get MothTale
tale times = do
  t <- threetale times
  r <- fourtale times
  s <- threetale times
  return $! MothTale
    t
    r
    s

threetale :: Integer -> Get Mothly3
threetale times = do
  v <- times ✿ f32x3 
  t <- times ✿ getFloatle
  return $! Mothly3
    v
    t
    
fourtale :: Integer -> Get Mothly4
fourtale times = do
  v <- times ✿ f32x4
  t <- times ✿ getFloatle
  return $! Mothly4
    v
    t
  
f32x4 :: Get (Vertex4 GLfloat)
f32x4 = do
  x <- getFloatle
  y <- getFloatle
  z <- getFloatle
  w <- getFloatle
  return $! Vertex4 x y z w

mothify :: Get MothFile
mothify = do
  bcount <- getWord8
  bones <- bcount ✿ exoskeleton

  let times = int bcount
      tell = times * 3
  tales <- tell ✿ tale times

  return $!
    MothFile 
      bcount 
      bones
      tales