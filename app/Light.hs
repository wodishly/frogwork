module Light where

import Data.Bifunctor (Bifunctor(bimap))

import Graphics.Rendering.OpenGL (ClearBuffer (ColorBuffer), Color4 (Color4), GLfloat, HasSetter (($=)), PrimitiveMode (Quads, Triangles), Vertex2 (Vertex2), clear, clearColor, drawArrays)

import Mean

type FrogColor = Color4 GLfloat
type Point = Vertex2 GLfloat
type Polygon = [Vertex2 GLfloat]

drawTriangle :: Polygon -> IO ()
drawTriangle triangle = do drawArrays Triangles 0 (fromIntegral $ length triangle)

drawFournook :: Polygon -> IO ()
drawFournook fournook = do drawArrays Quads 0 (fromIntegral $ length fournook)

dir :: GLfloat -> Point
dir = uncurry Vertex2 . bimap cos sin . twin

black :: FrogColor
black = Color4 0 0 0 1

white :: FrogColor
white = Color4 255 255 255 1

red :: FrogColor
red = Color4 255 0 0 1

green :: FrogColor
green = Color4 0 255 0 1

blue :: FrogColor
blue = Color4 0 0 255 1

yellow :: FrogColor
yellow = Color4 255 255 0 1

magenta :: FrogColor
magenta = Color4 255 0 255 1

-- interpolates a color `c` by fraction `n`
clerp :: GLfloat -> FrogColor -> FrogColor
clerp n = fmap (cast . (*) (clamp (0, 1) n) . cast)

bg :: FrogColor -> IO ()
bg c = clearColor $= c >> clear [ColorBuffer]
