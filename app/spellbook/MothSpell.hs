module MothSpell where

import Data.Binary.Get
import Foreign

import Numeric.LinearAlgebra
import Graphics.Rendering.OpenGL

import Matrix
import Spell
import Rime


data MothFile = MothFile {
  tomeCount :: Word8,
  boneCount :: Word8,
  skeleton :: [MothBone],
  library :: [MothTome]
} deriving (Show, Eq)

data MothTome = MothTome {
  duration :: GLfloat,
  chronicles :: [MothTale]
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
  v3 :: [Point3],
  t3 :: [GLfloat]
} deriving (Show, Eq)

data Mothly4 = Mothly4 {
  v4 :: [Point4],
  t4 :: [GLfloat]
} deriving (Show, Eq)

exoskeleton :: Get MothBone
exoskeleton = do
  parent <- u8
  floats <- 16 ✿ f32
  return $! MothBone parent $ (4><4) floats

tome :: Word8 -> Get MothTome
tome bonemany = do
  time <- f32
  let times = int bonemany
      tell = times
  tales <- tell ✿ tale
  return $! MothTome time tales

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
  t <- talemany ✿ f32
  v <- talemany ✿ f32x3 
  return $! Mothly3 v t
    
fourtale :: Get Mothly4
fourtale = do
  times <- s32
  let talemany = int times
  t <- talemany ✿ f32
  v <- talemany ✿ f32x4
  return $! Mothly4 v t

mothify :: Get MothFile
mothify = do
  tomemany <- u8
  bonemany <- u8
  bones <- bonemany ✿ exoskeleton
  tomes <- tomemany ✿ tome bonemany
  return $!
    MothFile 
      tomemany
      bonemany 
      bones
      tomes