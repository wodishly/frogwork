module Blee (
  white
, blue
, black
, clerp
, bg
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


type FrogColor = Color4 GLfloat

-- drawThreenook :: Polygon -> IO ()
-- drawThreenook triangle = do drawArrays Triangles 0 (fromIntegral $ length triangle)

-- drawFournook :: Polygon -> IO ()
-- drawFournook fournook = do drawArrays Quads 0 (fromIntegral $ length fournook)

-- evenNooks :: Int -> Polygon
-- evenNooks n = map (dir . (* (2*pi / cast n)). cast) (flight n)

black :: FrogColor
black = Color4 0 0 0 1

white :: FrogColor
white = Color4 255 255 255 1

-- red :: FrogColor
-- red = Color4 255 0 0 1

-- green :: FrogColor
-- green = Color4 0 255 0 1

blue :: FrogColor
blue = Color4 0 0 255 1

-- yellow :: FrogColor
-- yellow = Color4 255 255 0 1

-- magenta :: FrogColor
-- magenta = Color4 255 0 255 1

-- interpolates a color `c` by fraction `n`
clerp :: GLfloat -> FrogColor -> FrogColor
clerp = fmap . (*) . clamp (0.0, 1.0)

bg :: FrogColor -> StateT a IO ()
bg c = lift (clearColor $= c >> clear [ColorBuffer, DepthBuffer])
