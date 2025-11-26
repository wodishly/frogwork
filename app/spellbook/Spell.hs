module Spell where

import Control.Monad
import Data.Binary.Get

import Foreign
import Graphics.Rendering.OpenGL

import qualified Data.ByteString.Lazy as BL

import Rime


{-# INLINE (✿) #-}
(✿) :: Applicative m => Integral i => i -> m a -> m [a]
(✿) = replicateM . fromIntegral

{-# INLINE f32 #-}
f32 :: Get GLfloat
f32 = getFloatle

{-# INLINE s32 #-}
s32 :: Get Int32
s32 = getInt32le

{-# INLINE s16 #-}
s16 :: Get Int16
s16 = getInt16le

{-# INLINE u32 #-}
u32 :: Get Word32
u32 = getWord32le

{-# INLINE u8 #-}
u8 :: Get Word8
u8 = getWord8

f32x2 :: Get Point2
f32x2 = do
  u <- f32
  v <- f32
  return $! Vertex2 u v

f32x3 :: Get Point3
f32x3 = do
  x <- f32
  y <- f32
  z <- f32
  return $! Vertex3 x y z

f32x4 :: Get Point4
f32x4 = do
  x <- f32
  y <- f32
  z <- f32
  w <- f32
  return $! Vertex4 x y z w

u8x4 :: Get LatticePoint4
u8x4 = do
  x <- u8
  y <- u8
  z <- u8
  w <- u8
  return $! Vertex4
    (fromIntegral x) 
    (fromIntegral y) 
    (fromIntegral z) 
    (fromIntegral w)

{-# INLINE summon #-}
summon :: String -> IO BL.ByteString
summon = BL.readFile

{-# INLINE unwrappingly #-}
unwrappingly :: Get a -> BL.ByteString -> a
unwrappingly = runGet

{-# INLINE int #-}
int :: Integral i => i -> Integer
int = fromIntegral
