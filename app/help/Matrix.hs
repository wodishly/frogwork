module Matrix (
  Point
, Point2
, Point3
, Polygon
, Polyhedron
, FrogList
, FrogVector
, FrogMatrix
, RenderView (..)
, asFrog
, fromTranslation
, getProjectionMatrix
, frogZero -- unused?
, frogLookAt
, row -- unused
, norm
, norm3
, hat
, hat3
) where

import Numeric.LinearAlgebra (
    Element
  , Matrix
  , Vector
  , fromList
  , fromRows
  , asRow
  , asColumn
  , toList
  , cross
  , (><)
  , (|||)
  )

import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3))
import qualified SDL
import Foreign (Int32)


type Point = Vertex2 GLfloat
type Point2 = Point
type Point3 = Vertex3 GLfloat

type Polygon = [Point2]
type Polyhedron = [Point3]

type FrogList = [GLfloat]
type FrogVector = Vector GLfloat
type FrogMatrix = Matrix GLfloat

data RenderView = RenderView {
    _aspect :: GLfloat
  , _fov :: GLfloat
  , _near :: GLfloat
  , _far :: GLfloat
}

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

-- | Converts SDL's @P V2 Int32@ to OpenGL's @Vertex2 GLfloat@.
{-# INLINE asFrog #-}
asFrog :: SDL.Point SDL.V2 Int32 -> Point
asFrog (SDL.P (SDL.V2 x y)) = Vertex2 (fromIntegral x) (fromIntegral y)

{-# INLINE fromTranslation #-}
fromTranslation :: FrogList -> FrogMatrix
fromTranslation [x, y, z] = (4><4) [
  1, 0, 0, x,
  0, 1, 0, y,
  0, 0, 1, z,
  0, 0, 0, 1
  ]
fromTranslation _ = error "we need 3 dimensions"

{-# INLINE getProjectionMatrix #-}
getProjectionMatrix :: RenderView -> FrogMatrix
getProjectionMatrix (RenderView asp fov near far) = (4><4) [
   1/(asp*tan (fov/2)),             0,                     0,                     0
  ,                  0, 1/tan (fov/2),                     0,                     0
  ,                  0,             0, (near+far)/(near-far), 2*far*near/(near-far)
  ,                  0,             0,                    -1,                     0
  ]

{-# INLINE frogZero #-}
frogZero :: FrogVector
frogZero = fromList [0, 0, 0]

{-# INLINE frogUp #-}
frogUp :: FrogVector
frogUp = fromList [0, 1, 0]

frogLookAt :: FrogVector -> FrogVector -> FrogMatrix
frogLookAt eye target =
  let dir = normalize (eye - target)
      right = normalize (cross frogUp dir)
      up = cross dir right
      rotation = fromRows [right, up, dir, fromList (replicate 3 0)] ||| col [0, 0, 0, 1]
      translation = fromTranslation (toList -eye)
  in rotation <> translation


{-# INLINE row #-}
row :: Element t => [t] -> Matrix t
row = asRow.fromList

{-# INLINE col #-}
col :: Element t => [t] -> Matrix t
col = asColumn.fromList

{-# INLINE normalize #-}
normalize :: (Fractional t, Element t, Floating t) => Vector t -> Vector t
normalize v =
  let l = toList v
      d = sqrt $ sum $ map (^(2::Integer)) l
  in fromList (map (/d) l)
