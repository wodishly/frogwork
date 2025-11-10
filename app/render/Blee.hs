module Blee (
  white
, blue
, black
, lightwhelk
, darkwhelk
, clerp
, bg
, from255
) where

import Control.Monad.State (StateT, MonadTrans (lift))

import Graphics.Rendering.OpenGL (
    GLfloat
  , Color4 (Color4)
  , clearColor
  , ClearBuffer (..)
  , clear
  , ($=)
  )

import Rime (clamp)
import Mean ((<<))


type FrogColor = Color4 GLfloat

-- drawThreenook :: Polygon -> IO ()
-- drawThreenook triangle = do drawArrays Triangles 0 (fromIntegral $ length triangle)

-- drawFournook :: Polygon -> IO ()
-- drawFournook fournook = do drawArrays Quads 0 (fromIntegral $ length fournook)

-- evenNooks :: Int -> Polygon
-- evenNooks n = map (dir . (* (2*pi / cast n)). cast) (flight n)

from255 :: Int -> Int -> Int -> Int -> FrogColor
from255 = (((. f) .) .) . (((. f) .) . ((. f) . Color4) . f)
  where f = (/255) . fromIntegral

black :: FrogColor
black = Color4 0 0 0 1

white :: FrogColor
white = Color4 1 1 1 1

lightwhelk :: FrogColor
lightwhelk = from255 203 203 255 255

darkwhelk :: FrogColor
darkwhelk = from255 53 59 79 255

-- red :: FrogColor
-- red = Color4 1 0 0 1

-- green :: FrogColor
-- green = Color4 0 1 0 1

blue :: FrogColor
blue = Color4 0 0 1 1

-- yellow :: FrogColor
-- yellow = Color4 1 1 0 1

-- magenta :: FrogColor
-- magenta = Color4 1 0 1 1

-- interpolates a color `c` by fraction `n`
clerp :: GLfloat -> FrogColor -> FrogColor
clerp = fmap . (*) . clamp (0.0, 1.0)

bg :: FrogColor -> StateT a IO ()
bg = lift . (clear [ColorBuffer, DepthBuffer] <<) . (clearColor $=)
