module Spell (
  s32
, s16
, int
, f32
, f32x2
, f32x3
, f32x4
, u8
, u32
, summon
, (✿)
) where

import Control.Monad (replicateM)
import Data.Binary.Get (Get, getFloatle, getInt32le, getInt16le, getWord32le, getWord8)
import Foreign (Int16, Int32, Word32, Word8)
import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3), Vertex4 (Vertex4))
import qualified Data.ByteString.Lazy as BL

{-# INLINE (✿) #-}
(✿) :: Applicative m => Integral i => i -> m a -> m [a]
(✿) x = replicateM (fromIntegral x)

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

f32x2 :: Get (Vertex2 GLfloat)
f32x2 = do
  u <- f32
  v <- f32
  return $! Vertex2 u v

f32x3 :: Get (Vertex3 GLfloat)
f32x3 = do
  x <- f32
  y <- f32
  z <- f32
  return $! Vertex3 x y z

f32x4 :: Get (Vertex4 GLfloat)
f32x4 = do
  x <- f32
  y <- f32
  z <- f32
  w <- f32
  return $! Vertex4 x y z w

{-# INLINE summon #-}
summon :: String -> IO BL.ByteString
summon = BL.readFile

{-# INLINE int #-}
int :: Integral i => i -> Integer
int = fromIntegral
