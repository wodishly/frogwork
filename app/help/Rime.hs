{- HLINT ignore "Use head" -}
module Rime where

import Data.Functor ((<&>))
import Numeric.LinearAlgebra (fromList, toList)

import Foreign (Int32)
import Graphics.Rendering.OpenGL (
    GLfloat
  , GLint
  )

import qualified SDL (Point (P), V2 (V2))
import qualified Numeric.LinearAlgebra as H (Vector, size)
import qualified Graphics.Rendering.OpenGL as GL (
    Vertex
  , Vertex2 (Vertex2)
  , Vertex3 (Vertex3)
  , Vertex4 (Vertex4)
  , VertexComponent
  )

import Mean (toBoth, sq, dimensionError, ssss)


infixl 7 *^, ^*, /^, ^/
infixl 6 <+>, <->

type Point2 = GL.Vertex2 GLfloat
type Point3 = GL.Vertex3 GLfloat
type Point4 = GL.Vertex4 GLfloat
type Point = Point2

type Polygon = [Point2]
type Polyhedron = [Point3]
type Polytope = [Point4]

type LatticePoint4 = GL.Vertex4 GLint
type ZPolytope = [LatticePoint4]

type FrogList = [GLfloat]
type FrogVector = H.Vector GLfloat

data Fournook = Fournook {
  topLeft :: Point
, greatness :: Point
} deriving (Show, Eq)

{-# INLINE asPoint #-}
asPoint :: Real a => a -> Point
asPoint = toBoth GL.Vertex2 . realToFrac

{-# INLINE average #-}
average :: Real a => [a] -> GLfloat
average = ssss ((/) . realToFrac . sum) (fromIntegral.length)

{-# INLINE clamp #-}
clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) = min high . max low

class GL.Vertex v => FrogVertex v where
-- | Shapeshifts SDL's @P V2 Int32@ to OpenGL's @Vertex2 GLfloat@.
  fromSDL :: SDL.Point SDL.V2 Int32 -> v

-- | Shapeshifts OpenGL's @Vertex GLfloat@ to hmatrix's @Vector GLfloat@.
  toFrogList :: v -> FrogList

-- | Shapeshifts OpenGL's @Vertex GLfloat@ to hmatrix's @Vector GLfloat@.
  {-# INLINE toFrogVector #-}
  toFrogVector :: v -> FrogVector
  toFrogVector = fromList . toFrogList

-- | Shapeshifts hmatrix's @Vector GLfloat@ to OpenGL's @Vertex GLfloat@.
  fromFrogVector :: FrogVector -> v

  hat :: FrogVertex v => v -> v
  nonehood :: v
  onehood :: v

  -- | componentwise multiplication
  (^*^) :: v -> v -> v

  {-# INLINE norm #-}
  norm :: FrogVertex v => v -> GLfloat
  norm = sqrt . sum . map sq . toFrogList

  {-# INLINE isNought #-}
  isNought :: FrogVertex v => v -> Bool
  isNought = (== 0) . norm

  {-# INLINE isAught #-}
  isAught :: FrogVertex v => v -> Bool
  isAught = not.isNought

instance (RealFrac a, GL.VertexComponent a) => FrogVertex (GL.Vertex2 a) where
  {-# INLINE fromSDL #-}
  fromSDL (SDL.P (SDL.V2 x y)) = fromIntegral <$> GL.Vertex2 x y
  {-# INLINE toFrogList #-}
  toFrogList (GL.Vertex2 x y) = realToFrac <$> [x, y]
  {-# INLINE fromFrogVector #-}
  fromFrogVector v
    | H.size v == 2 = let l = realToFrac <$> toList v in GL.Vertex2 (l!!0) (l!!1)
    | otherwise = dimensionError 2
  {-# INLINE (^*^) #-}
  (^*^) (GL.Vertex2 x y) (GL.Vertex2 a b) = GL.Vertex2 (x*a) (y*b)
  {-# INLINE nonehood #-}
  nonehood = GL.Vertex2 0 0
  {-# INLINE onehood #-}
  onehood = GL.Vertex2 1 1
  {-# INLINE hat #-}
  hat z
    | isNought z = z
    | otherwise = (/realToFrac (norm z)) <$> z

instance (RealFrac a, GL.VertexComponent a) => FrogVertex (GL.Vertex3 a) where
  {-# INLINE fromSDL #-}
  fromSDL (SDL.P (SDL.V2 x y)) = fromIntegral <$> GL.Vertex3 x y 0
  {-# INLINE toFrogList #-}
  toFrogList (GL.Vertex3 x y z) = realToFrac <$> [x, y, z]
  {-# INLINE fromFrogVector #-}
  fromFrogVector v
    | H.size v == 3 = let l = realToFrac <$> toList v in GL.Vertex3 (l!!0) (l!!1) (l!!2)
    | otherwise = dimensionError 3
  {-# INLINE (^*^) #-}
  (^*^) (GL.Vertex3 x y z) (GL.Vertex3 a b c) = GL.Vertex3 (x*a) (y*b) (z*c)
  {-# INLINE nonehood #-}
  nonehood = GL.Vertex3 0 0 0
  {-# INLINE onehood #-}
  onehood = GL.Vertex3 1 1 1
  {-# INLINE hat #-}
  hat z
    | isNought z = z
    | otherwise = (/realToFrac (norm z)) <$> z

instance (RealFrac a, GL.VertexComponent a) => FrogVertex (GL.Vertex4 a) where
  {-# INLINE fromSDL #-}
  fromSDL (SDL.P (SDL.V2 x y)) = fromIntegral <$> GL.Vertex4 x y 0 0
  {-# INLINE toFrogList #-}
  toFrogList (GL.Vertex4 x y z w) = realToFrac <$> [x, y, z, w]
  {-# INLINE fromFrogVector #-}
  fromFrogVector v
    | H.size v == 4 = let l = realToFrac <$> toList v in GL.Vertex4 (l!!0) (l!!1) (l!!2) (l!!3)
    | otherwise = dimensionError 3
  {-# INLINE (^*^) #-}
  (^*^) (GL.Vertex4 x y z w) (GL.Vertex4 a b c d) = GL.Vertex4 (x*a) (y*b) (z*c) (w*d)
  {-# INLINE nonehood #-}
  nonehood = GL.Vertex4 0 0 0 0
  {-# INLINE onehood #-}
  onehood = GL.Vertex4 1 1 1 1
  {-# INLINE hat #-}
  hat z
    | isNought z = z
    | otherwise = (/realToFrac (norm z)) <$> z

{-# INLINE (<+>) #-}
(<+>) :: (Applicative f, Num a) => f a -> f a -> f a
(<+>) = liftA2 (+)

{-# INLINE (<->) #-}
(<->) :: (Applicative f, Num a) => f a -> f a -> f a
(<->) = liftA2 (-)

{-# INLINE (*^) #-}
(*^) :: (Applicative f, Num a) => a -> f a -> f a
(*^) = (<$>) . (*)

{-# INLINE (^*) #-}
(^*) :: (Applicative f, Num a) => f a -> a -> f a
(^*) = flip (*^)

{-# INLINE (/^) #-}
(/^) :: (Applicative f, Fractional a) => a -> f a -> f a
(/^) = (<$>) . (/)

{-# INLINE (^/) #-}
(^/) :: (Functor f, Fractional a) => f a -> a -> f a
(^/) = (. flip (/)) . (<&>)

{-# INLINE swizzle #-}
swizzle :: GL.Vertex4 a -> GL.Vertex4 a
swizzle (GL.Vertex4 y z real x) = GL.Vertex4 x y z real

{-# INLINE unswizzle #-}
unswizzle :: GL.Vertex4 a -> GL.Vertex4 a
unswizzle (GL.Vertex4 x y z real) = GL.Vertex4 y z real x

-- | gimme the southeast and the northwest
--
-- use @inject@ if u need another dimension
fournook :: Point -> Point -> Polygon
fournook se@(GL.Vertex2 x y) nw@(GL.Vertex2 a b) = [
    se
  , GL.Vertex2 x b -- NE
  , nw
  , GL.Vertex2 a y -- SW
  ]

data Axle = X | Y | Z deriving (Show, Eq)

-- | gimme an axle and a 2d point and i give u a 3d point with that axle set to 0
inject :: Axle -> Point2 -> Point3
inject axle (GL.Vertex2 x y) = case axle of
  X -> GL.Vertex3 0 x y
  Y -> GL.Vertex3 x 0 y
  Z -> GL.Vertex3 x y 0

-- | gimme an axle and a 3d point and i give u a 2d point without that axle
surject :: Axle -> Point3 -> Point2
surject axle (GL.Vertex3 x y z) = case axle of
  X -> GL.Vertex2 x y
  Y -> GL.Vertex2 x z
  Z -> GL.Vertex2 y z
