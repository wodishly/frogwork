{-# OPTIONS_GHC -Wno-type-defaults #-}
module MothSpell where

import Data.Binary.Get (Get)
import Foreign (Word8)
import Graphics.Rendering.OpenGL (GLfloat, Vertex4 (..))
import Graphics.Rendering.OpenGL.GL (Vertex3)

import Numeric.LinearAlgebra.HMatrix ((><))
import Matrix (FrogMatrix)

import Spell ((✿), int, u8, s32, f32, f32x3, f32x4)

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
  parent <- u8
  floats <- 16 ✿ f32
  return $! MothBone parent $ (4><4) floats

tale :: Get MothTale
tale = do
  t <- threetale
  r <- fourtale
  s <- threetale
  return $! MothTale t r s

threetale :: Get Mothly3
threetale = do
  times <- s32
  let talemany = int times
  v <- talemany ✿ f32x3 
  t <- talemany ✿ f32
  return $! Mothly3 v t
    
fourtale :: Get Mothly4
fourtale = do
  times <- s32
  let talemany = int times
  v <- talemany ✿ f32x4
  t <- talemany ✿ f32
  return $! Mothly4 v t

mothify :: Get MothFile
mothify = do
  bonemany <- u8
  bones <- bonemany ✿ exoskeleton

  let times = int bonemany
      tell = times
  tales <- tell ✿ tale

  return $!
    MothFile 
      bonemany 
      bones
      tales