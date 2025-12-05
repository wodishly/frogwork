module Strike where

import Mean
import Rime
import Matrix


-- | A spit is an axle between a @Point3@ and the farthest @Point3@.
type Spit = Twain Point3

-- | aye
frame' :: Spit -> Polyhedron
frame' (l@(Vertex3 x0 y0 z0), Vertex3 x1 y1 z1)
  = (l <+>) <$> [
        -- front bottom of spit
        Vertex3 x0 y0 z1,
        Vertex3 x1 y0 z1,
        -- front top of spit
        Vertex3 x1 y1 z1,
        Vertex3 x0 y1 z1,
        -- back bottom of spit
        Vertex3 x0 y0 z0,
        Vertex3 x1 y0 z0,
        -- back top of spit
        Vertex3 x1 y1 z0,
        Vertex3 x0 y1 z0
      ]

striketh :: Spit -> Spit -> Bool
striketh (Vertex3 x0 y0 z0, Vertex3 x1 y1 z1) (Vertex3 a0 b0 c0, Vertex3 a1 b1 c1) =
    x0 <= a1 && x1 >= a0
 && y0 <= b1 && y1 >= b0
 && z0 <= c1 && z1 >= c0

shapeshiftFrame :: Spit -> Point3 -> FrogMatrix
shapeshiftFrame (left@(Vertex3 a b c), right) (Vertex3 x y z) =
  fromAffine
  [dx, dy, dz]
  (zipWith (+) [-(dx/2), 0.1, -(dz/2)] [a+x, b+y, c+z])
  where Vertex3 dx dy dz = right <-> left
