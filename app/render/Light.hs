module Light where

import Data.Bifunctor (bimap)
import Foreign (Int32, Ptr, nullPtr, plusPtr)
import Graphics.Rendering.OpenGL

import Mean
import Rime

type FrogColor = Color4 GLfloat
type Point = Vertex2 GLfloat
type Polygon = [Vertex2 GLfloat]
type Polyhedron = [Vertex3 GLfloat]
type Faces = [Int]

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr . fromIntegral

drawThreenook :: Polygon -> IO ()
drawThreenook triangle = do drawArrays Triangles 0 (fromIntegral $ length triangle)

drawFournook :: Polygon -> IO ()
drawFournook fournook = do drawArrays Quads 0 (fromIntegral $ length fournook)

drawFaces :: Int32 -> IO ()
drawFaces count = do drawElements Triangles count UnsignedInt (bufferOffset 0)

evenNooks :: Int -> Polygon
evenNooks n = map (dir . (* (2*pi / cast n)). cast) (flight n)

-- radians
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
