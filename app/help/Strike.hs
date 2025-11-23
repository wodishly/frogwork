module Strike where

import Graphics.Rendering.OpenGL (Vertex3 (Vertex3))

import Mean (Twain, between, ly)
import Rime (Axle (..), Point3, Polyhedron, (<+>))


-- | A spit is an axle between a @Point3@ and the farthest @Point3@.
type Spit = Twain Point3

frame' :: Spit -> Polyhedron
frame' (w0@(Vertex3 x0 y0 z0), Vertex3 x1 y1 z1)
  = ly $ (w0 <+>) <$> [
        Vertex3 x0 y0 z0,
        Vertex3 x0 y0 z1,
        Vertex3 x0 y1 z1,
        Vertex3 x0 y1 z0,
        Vertex3 x1 y1 z0,
        Vertex3 x1 y1 z1,
        Vertex3 x1 y0 z1,
        Vertex3 x1 y0 z0
      ]

-- | To be spitful is to have a spit, and hence also a frame.
class Spitful a where
  spit :: a -> Spit

  frame :: a -> Polyhedron
  frame = frame' . spit

  striketh :: Spitful b => a -> b -> Bool
  striketh struck striking = and (strikethAlong <$> [X, Y, Z] <*> replicate 3 struck <*> replicate 3 striking)

  strikethAlong :: Spitful b => Axle -> a -> b -> Bool
  strikethAlong axle struck striking = betweenAlong axle struck (snd $ spit striking)
                                    && betweenAlong axle striking (fst $ spit struck)

  betweenAlong ::  Axle -> a -> Point3 -> Bool
  betweenAlong X a (Vertex3 x _ _) = between (x0, x1) x where (Vertex3 x0 _ _, Vertex3 x1 _ _) = spit a
  betweenAlong Y a (Vertex3 _ y _) = between (y0, y1) y where (Vertex3 _ y0 _, Vertex3 _ y1 _) = spit a
  betweenAlong Z a (Vertex3 _ _ z) = between (z0, z1) z where (Vertex3 _ _ z0, Vertex3 _ _ z1) = spit a
