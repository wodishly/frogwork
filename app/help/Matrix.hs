module Matrix where

import Data.Matrix as M
import qualified Data.Vector.Storable as S
import Graphics.Rendering.OpenGL (GLfloat)
import Data.Vector.Generic (convert)

type FrogMatrix = Matrix GLfloat
type Transform = S.Vector GLfloat

-- Computations should be done using @FrogMatrix@ as much as possible,
-- and then converted to @Transform@ at the last minute.
-- Since @FrogMatrix@ is an alias for @Data.Matrix.Matrix a@,
-- the operations (==), (+), (-), (*) can all be used for free.

-- | Converts a list directly into a @S.Vector@,
-- bypassing the @M.Matrix@ representation.
{-# INLINE hew' #-}
hew' :: [GLfloat] -> Transform
hew' = S.fromList

-- | Converts a @M.Matrix@ into a @S.Vector@.
{-# INLINE hew #-}
hew :: FrogMatrix -> Transform
hew = convert . getMatrixAsVector

-- | Converts an @S.Vector@ into a @M.Matrix@.
--
-- Throws if the vector is not made up of 16 @GLfloat@s.
--
-- There may be a faster implementation.
{-# INLINE unhew #-}
unhew :: Transform -> FrogMatrix
unhew = M.fromList 4 4 . S.toList

identity :: Transform
identity = hew $ M.identity 4

fromTranslation :: GLfloat -> GLfloat -> GLfloat -> Transform
fromTranslation x y z = hew' [
  1,0,0,x,
  0,1,0,y,
  0,0,1,z,
  0,0,0,1
  ]

data RenderView = RenderView {
  _aspect :: GLfloat,
  _fov :: GLfloat,
  _near :: GLfloat,
  _far :: GLfloat
}

getProjectionMatrix :: RenderView -> Transform
getProjectionMatrix (RenderView aspect fov near far) = hew' [
  1 / (aspect * tan (fov / 2)), 0.0, 0.0, 0.0,
  0.0, 1 / tan (fov / 2), 0.0, 0.0,
  0.0, 0.0,- ((far + near) / (far - near)),- (2.0 * far * near / (far - near)),
  0.0, 0.0, -1.0, 0.0
  ]
