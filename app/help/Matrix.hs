{- HLINT ignore "Use head" -}
module Matrix where

import Numeric.LinearAlgebra as H (
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
  , (|||), size
  )

import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3), Vertex, VertexComponent)

import qualified SDL (Point (P), V2 (V2))
import Foreign (Int32)


-- * On the sundering of rimes.
-- 
-- $sundering
-- All earthcraft ("geometry") is split into three deals,
--   one of which SDL brooks ("uses") for happenings ("events"),
--   another OpenGL for drawing,
--   the third that which in its own tongue is called Numeric.LinearAlgebra, in ours hmatrix.
-- These all are of sundry shape and meaning.
--
-- No reckoning ("computation") should be done with SDL. Any rime ("number")
--   begotten thereof should be cast at once, by way of @fromSDL@, to be of OpenGL.
-- Anything to be drawn must, at the end of its fare, be of hmatrix,
--   either built by hand or cast by way of @toVector@.
-- All other reckoning should be of OpenGL; here @fromVector@ is given
--   for the cast, however seldseen, of a rime of hmatrix back to OpenGL.
--
-- The yokes ("types") of earthcraft follow as follows:
--   Begotten of SDL alone:
--     - V2 Int32
--     - Point V2 Int32
--   Begotten of OpenGL alone:
--     - GLfloat
--     - [GLfloat], also called FrogList
--     - Vertex2 GLfloat, also called Point2, also called Point
--     - Vertex3 GLfloat, also called Point3
--     - [Vertex2 GLfloat], also called Polygon
--     - [Vertex3 GLfloat], also called Polyhedron
--   Begotten of OpenGL and hmatrix:
--     - Vector GLfloat, also called FrogVector
--     - Matrix GLfloat, also called FrogMatrix
--
-- Godspeed.

class Vertex v => FrogVertex v where
-- | Shapeshifts SDL's @P V2 Int32@ to OpenGL's @Vertex2 GLfloat@.
  fromSDL :: SDL.Point SDL.V2 Int32 -> v
-- | Shapeshifts OpenGL's @Vertex GLfloat@ to hmatrix's @Vector GLfloat@.
  toVector :: v -> FrogVector
-- | Shapeshifts hmatrix's @Vector GLfloat@ to OpenGL's @Vertex GLfloat@.
  fromVector :: FrogVector -> v

instance (RealFrac a, VertexComponent a) => FrogVertex (Vertex2 a) where
  {-# INLINE fromSDL #-}
  fromSDL (SDL.P (SDL.V2 x y)) = fromIntegral <$> Vertex2 x y
  {-# INLINE toVector #-}
  toVector (Vertex2 x y) = fromList $ realToFrac <$> [x, y]
  {-# INLINE fromVector #-}
  fromVector v
    | H.size v == 2 = let l = realToFrac <$> toList v in Vertex2 (l!!0) (l!!1)
    | otherwise = error "bad bad"

instance (RealFrac a, VertexComponent a) => FrogVertex (Vertex3 a) where
  {-# INLINE fromSDL #-}
  fromSDL (SDL.P (SDL.V2 x y)) = fromIntegral <$> Vertex3 x y 0
  {-# INLINE toVector #-}
  toVector (Vertex3 x y z) = fromList $ realToFrac <$> [x, y, z]
  {-# INLINE fromVector #-}
  fromVector v
    | H.size v == 3 = let l = realToFrac <$> toList v in Vertex3 (l!!0) (l!!1) (l!!2)
    | otherwise = error "bad bad bad"

type FrogList = [GLfloat]
type FrogVector = Vector GLfloat
type FrogMatrix = Matrix GLfloat

data RenderView = RenderView {
    aspect :: GLfloat
  , size :: (GLfloat, GLfloat)
  , fov :: GLfloat
  , near :: GLfloat
  , far :: GLfloat
}

{-# INLINE fromTranslation #-}
fromTranslation :: FrogList -> FrogMatrix
fromTranslation [x, y, z] = (4><4) [
  1, 0, 0, x,
  0, 1, 0, y,
  0, 0, 1, z,
  0, 0, 0, 1
  ]
fromTranslation _ = error "we need 3 dimensions"

{-# INLINE getPerspectiveMatrix #-}
getPerspectiveMatrix :: RenderView -> FrogMatrix
getPerspectiveMatrix (RenderView asp _ fov' near' far') = (4><4) [
   1/(asp*tan (fov'/2)),              0,                         0,                         0
  ,                   0, 1/tan (fov'/2),                         0,                         0
  ,                   0,              0, (near'+far')/(near'-far'), 2*far'*near'/(near'-far')
  ,                   0,              0,                        -1,                         0
  ]

{-# INLINE getOrthographicMatrix #-}
getOrthographicMatrix :: RenderView -> FrogMatrix
getOrthographicMatrix (RenderView _ (w, h) _ _ _) = (4><4) [
    2/w, 0,   0, -1
  , 0,   2/h, 0, -1
  , 0,   0,  -1,  0
  , 0,   0,   0,  1
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
