module Rime where

import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3))

import Mean (doBoth)


type Point2 = Vertex2 GLfloat
type Point3 = Vertex3 GLfloat
type Point = Point2

type Polygon = [Point2]
type Polyhedron = [Point3]

{-# INLINE average #-}
average :: Real a => [a] -> GLfloat
average = uncurry (/) . doBoth (realToFrac.sum) (fromIntegral.length)

{-# INLINE clamp #-}
clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) = min high . max low

{-# INLINE nought #-}
nought :: Point -> Bool
nought (Vertex2 x y) = x == 0 && y == 0

{-# INLINE aught #-}
aught :: Point -> Bool
aught = not.nought

{-# INLINE norm #-}
norm :: Point -> GLfloat
norm (Vertex2 x y) = sqrt (x*x + y*y)

{-# INLINE norm3 #-}
norm3 :: Point3 -> GLfloat
norm3 (Vertex3 x y z) = sqrt (x*x + y*y + z*z)

{-# INLINE hat #-}
hat :: Point -> Point
hat z
  | norm z == 0 = z
  | otherwise = fmap (/norm z) z

{-# INLINE hat3 #-}
hat3 :: Point3 -> Point3
hat3 z
  | norm3 z == 0 = z
  | otherwise = fmap (/norm3 z) z

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

