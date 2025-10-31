module Frog where

import Light
import Graphics.Rendering.OpenGL
import Control.Lens.Internal.CTypes (Word32, Word8)

makeFrog :: Polygon
makeFrog = map (fmap (/8) . dir) [pi/2, 7*pi/6, 11*pi/6]

make3dFrog :: Polyhedron
make3dFrog = [Vertex3 (-0.067344) 0.195526 (-0.304172)]

make3dFrogIndices :: [Word32]
make3dFrogIndices = [0, 0, 0]

make3dFrogUvs :: [Vertex2 GLfloat]
make3dFrogUvs = [Vertex2 0.168098 0.549061]

make3dFrogBitmap :: [Word8]
make3dFrogBitmap = [255,255,255,255]