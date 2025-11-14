module Blee (
  Blee
, white
, red
, green
, blue
, black
, lightwhelk
, darkwhelk
, clerp
, bg
, from255
, bleeToGLVector4
) where

import Control.Monad.State (StateT, MonadTrans (lift))

import Graphics.Rendering.OpenGL (
    ClearBuffer (..)
  , Color4 (Color4)
  , GLfloat
  , clear
  , clearColor
  , Vector4 (Vector4), HasSetter (($=))
  )

import Rime (clamp)


type Blee = Color4 GLfloat

-- drawThreenook :: Polygon -> IO ()
-- drawThreenook triangle = do drawArrays Triangles 0 (fromIntegral $ length triangle)

-- drawFournook :: Polygon -> IO ()
-- drawFournook fournook = do drawArrays Quads 0 (fromIntegral $ length fournook)

-- evenNooks :: Int -> Polygon
-- evenNooks n = map (dir . (* (2*pi / cast n)). cast) (flight n)

from255 :: Int -> Int -> Int -> Int -> Blee
from255 = (((. f) .) .) . (((. f) .) . ((. f) . Color4) . f)
  where f = (/255) . fromIntegral

from256 :: Int -> Int -> Int -> Int -> Blee
from256 = (((. f) .) .) . (((. f) .) . ((. f) . Color4) . f)
  where f = (/255) . (- 1) . fromIntegral

bleeToGLVector4 :: Blee -> Vector4 GLfloat
bleeToGLVector4 (Color4 r g b a) = Vector4 r g b a

black :: Blee
black = Color4 0 0 0 1

white :: Blee
white = Color4 1 1 1 1

lightwhelk :: Blee
lightwhelk = from256 204 204 256 256

darkwhelk :: Blee
darkwhelk = from256 54 60 80 256

red :: Blee
red = Color4 1 0 0 1

green :: Blee
green = Color4 0 1 0 1

blue :: Blee
blue = Color4 0 0 1 1

-- yellow :: FrogColor
-- yellow = Color4 1 1 0 1

-- magenta :: FrogColor
-- magenta = Color4 1 0 1 1

-- interpolates a color `c` by fraction `n`
clerp :: GLfloat -> Blee -> Blee
clerp = fmap . (*) . clamp (0, 1)

bg :: Blee -> StateT a IO ()
bg = lift . (>> clear [ColorBuffer, DepthBuffer]) . (clearColor $=)
