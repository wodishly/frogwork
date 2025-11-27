{- HLINT ignore "Use head" -}
module Matrix (
  module Matrix,
  module H,
) where

import Prelude hiding ((<>))

import Numeric.LinearAlgebra as H hiding (normalize, scale, step, format)

import Mean
import Rime


-- * On the sundering of rimes.
-- 
-- $sundering
-- All earthcraft ("geometry") is split into three deals,
--   one of which SDL brooks ("uses") for happenings ("events"),
--   another OpenGL for drawing,
--   the third that which in its own tongue is called Numeric.LinearAlgebra, in ours hmatrix.
-- These all are of sundry shape and meaning.
--
-- No reckoning ("computation") should be done with SDL.
--   Any rime ("number") begotten thereof should be cast at once,
--   by way of @fromSDL@, to be of OpenGL.
-- Anything to be drawn must, at the end of its fare, be of hmatrix,
--   either built by hand or cast by way of @toFrogVector@.
-- All other reckoning should be of OpenGL; here @fromFrogVector@ is given
--   for the cast, however seldseen, of a rime of hmatrix back to OpenGL.
--
-- The yokes ("types") of earthcraft follow as follows:
--   Begotten of SDL alone:
--     - @V2 Int32@
--     - @Point V2 Int32@
--     * Mind that @Point@ and @V2@ are from Linear.Affine and Linear.V2.
--   Begotten of OpenGL alone:
--     - @GLfloat@
--     - @[GLfloat]@, also called FrogList
--     - @Vertex2 GLfloat@, also called @Point2@, also called @Point@
--     - @Vertex3 GLfloat@, also called @Point3@
--     - @[Vertex2 GLfloat]@, also called @Polygon@
--     - @[Vertex3 GLfloat]@, also called @Polyhedron@
--   Begotten of OpenGL and hmatrix:
--     - @Vector GLfloat@, also called @FrogVector@
--     - @Matrix GLfloat@, also called @FrogMatrix@
--     * Mind that @Vector@ is from Data.Vector.Storable.
--
-- Godspeed.

type FrogMatrix = H.Matrix GLfloat

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
fromTranslation _ = dimensionError 3

{-# INLINE fromAffine #-}
fromAffine :: FrogList -> FrogList -> FrogMatrix
fromAffine [s, t, u] [x, y, z] = (4><4) [
  s, 0, 0, x,
  0, t, 0, y,
  0, 0, u, z,
  0, 0, 0, 1
  ]
fromAffine _ _ = dimensionError 3

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

frogLookAt :: FrogVector -> FrogVector -> FrogMatrix
frogLookAt eye target =
  let dir = frogNormalize (eye - target)
      right = frogNormalize (cross (fromList [0, 1, 0]) dir)
      up = cross dir right
      rotation = fromRows [right, up, dir, fromList (replicate 3 0)] ||| frogCol [0, 0, 0, 1]
      translation = fromTranslation (toList -eye)
  in rotation <> translation

{-# INLINE frogRow #-}
frogRow :: H.Element t => [t] -> H.Matrix t
frogRow = asRow . fromList

{-# INLINE frogCol #-}
frogCol :: H.Element t => [t] -> H.Matrix t
frogCol = asColumn . fromList

{-# INLINE frogNormalize #-}
frogNormalize :: (Fractional t, H.Element t, Floating t) => H.Vector t -> H.Vector t
frogNormalize v =
  let l = toList v
      d = sqrt $ sum $ map sq l
  in fromList (map (/d) l)
