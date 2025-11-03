module Matrix where
import Graphics.Rendering.OpenGL (GLfloat)

import Data.Matrix as M
import Data.Vector.Generic (convert)
import qualified Data.Vector.Storable as S


type FrogVector = [GLfloat]
type FrogMatrix = Matrix GLfloat
type StorableMatrix = S.Vector GLfloat

-- Computations should be done using @FrogMatrix@ as much as possible,
-- and then converted to @StorableMatrix@ at the last minute.
-- Since @FrogMatrix@ is an alias for @Data.Matrix.Matrix a@,
-- the operations (==), (+), (-), (*) can all be used for free.

-- | Converts a list directly into a @S.Vector@,
-- bypassing the @M.Matrix@ representation.
{-# INLINE hew' #-}
hew' :: [GLfloat] -> StorableMatrix
hew' = S.fromList

-- | Converts a @M.Matrix@ into a @S.Vector@.
{-# INLINE hew #-}
hew :: FrogMatrix -> StorableMatrix
hew = convert . getMatrixAsVector

-- | Converts an @S.Vector@ into a @M.Matrix@.
--
-- Throws if the vector is not made up of 16 @GLfloat@s.
--
-- There may be a faster implementation.
{-# INLINE unhew #-}
unhew :: StorableMatrix -> FrogMatrix
unhew = M.fromList 4 4 . S.toList

{-# INLINE identityMatrix #-}
identityMatrix :: FrogMatrix
identityMatrix = M.identity 4

{-# INLINE identity #-}
identity :: StorableMatrix
identity = hew $ M.identity 4

{-# INLINE fromTranslation #-}
fromTranslation :: FrogVector -> FrogMatrix
fromTranslation [x, y, z] = M.fromList 4 4 [
  1,0,0,x,
  0,1,0,y,
  0,0,1,z,
  0,0,0,1
  ]
fromTranslation _ = identityMatrix
data RenderView = RenderView {
  _aspect :: GLfloat,
  _fov :: GLfloat,
  _near :: GLfloat,
  _far :: GLfloat
}

getProjectionMatrix :: RenderView -> StorableMatrix
getProjectionMatrix (RenderView aspect fov near far) = hew' [
  1 / (aspect * tan (fov / 2)), 0.0, 0.0, 0.0,
  0.0, 1 / tan (fov / 2), 0.0, 0.0,
  0.0, 0.0,- ((far + near) / (far - near)),- (2.0 * far * near / (far - near)),
  0.0, 0.0, -1.0, 0.0
  ]

cross :: FrogVector -> FrogVector -> FrogVector
cross [ax, ay, az] [bx, by, bz] = [
  ay * bz - az * by,
  az * bx - ax * bz,
  ax * by - ay * bx
  ]
cross _ _ = frogZero

normalize :: FrogVector -> FrogVector
normalize [x,y,z] = let d = sqrt(x*x + y*y + z*z) in
  [x / d, y / d, z / d]
normalize _ = frogZero

frogZero :: FrogVector
frogZero = [0, 0, 0]

frogUp :: FrogVector
frogUp = [0, 1, 0]

sub :: FrogVector -> FrogVector -> FrogVector
sub [a, b, c] [d, e, f] = [a - d, b - e, c - f]
sub _ _ = frogZero

frogLookAt :: FrogVector -> FrogVector -> StorableMatrix
frogLookAt eye target =
  let dir = normalize (sub target eye)
      right = normalize (cross frogUp dir)
      up = cross dir right
  in hew $ setElem 1 (4, 4) 
    (extendTo 0 4 4 (M.fromLists [right,up,dir])) * fromTranslation (sub frogZero eye)
