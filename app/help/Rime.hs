module Rime where

import Graphics.Rendering.OpenGL (GLfloat, GLint, Vertex2, Vertex3, Vertex4)

import Mean (doBoth)


type Point2 = Vertex2 GLfloat
type Point3 = Vertex3 GLfloat
type Point4 = Vertex4 GLfloat
type Point = Point2

type Polygon = [Point2]
type Polyhedron = [Point3]
type Polytope = [Point4]

type LatticePoint4 = Vertex4 GLint
type ZPolytope = [LatticePoint4]

{-# INLINE average #-}
average :: Real a => [a] -> GLfloat
average = uncurry (/) . doBoth (realToFrac.sum) (fromIntegral.length)

{-# INLINE clamp #-}
clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) = min high . max low

infixl 7 *^, ^*
infixl 6 <+>

{-# INLINE (<+>) #-}
(<+>) :: (Applicative f, Num a) => f a -> f a -> f a
(<+>) = liftA2 (+)

{-# INLINE (*^) #-}
(*^) :: (Applicative f, Num a) => a -> f a -> f a
(*^) = (<$>) . (*)

{-# INLINE (^*) #-}
(^*) :: (Applicative f, Num a) => f a -> a -> f a
(^*) = flip (*^)

