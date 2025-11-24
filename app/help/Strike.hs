{-# LANGUAGE TypeSynonymInstances #-}
module Strike where

import Graphics.Rendering.OpenGL (Vertex3 (Vertex3))

import Mean (Twain, between)
import Rime (Axle (..), Point3, Polyhedron, (<+>), FrogVertex (toFrogList), (<->))
import Matrix (FrogMatrix, fromAffine)


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

spitteth :: Spit -> Spit -> Bool
spitteth struck striking = and (spittethAlong <$> [X, Y, Z] <*> replicate 3 struck <*> replicate 3 striking)

spittethAlong :: Axle -> Spit -> Spit -> Bool
spittethAlong axle spitted spitting = betweenAlong axle spitted (snd spitting)
                                   && betweenAlong axle spitting (fst spitted)

betweenAlong ::  Axle -> Spit -> Point3 -> Bool
betweenAlong X spit (Vertex3 x _ _) = between (x0, x1) x where (Vertex3 x0 _ _, Vertex3 x1 _ _) = spit
betweenAlong Y spit (Vertex3 _ y _) = between (y0, y1) y where (Vertex3 _ y0 _, Vertex3 _ y1 _) = spit
betweenAlong Z spit (Vertex3 _ _ z) = between (z0, z1) z where (Vertex3 _ _ z0, Vertex3 _ _ z1) = spit

shapeshiftFrame :: Spit -> FrogMatrix
shapeshiftFrame spit = fromAffine [x, y, z] (zipWith (+) [-(x/2), 0.1, -(z/2)] (toFrogList $ fst spit))
  where
  Vertex3 x y z = abs <$> uncurry (<->) spit
