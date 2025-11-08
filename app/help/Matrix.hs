module Matrix where

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

import Graphics.Rendering.OpenGL (GLfloat)


type FrogList = [GLfloat]
type FrogVector = Vector GLfloat
type FrogMatrix = Matrix GLfloat

{-# INLINE fromTranslation #-}
fromTranslation :: FrogList -> FrogMatrix
fromTranslation [x, y, z] = (4><4) [
  1,0,0,x,
  0,1,0,y,
  0,0,1,z,
  0,0,0,1
  ]
fromTranslation _ = error "no"

data RenderView = RenderView {
    _aspect :: GLfloat
  , _fov :: GLfloat
  , _near :: GLfloat
  , _far :: GLfloat
}

getProjectionMatrix :: RenderView -> FrogMatrix
getProjectionMatrix (RenderView asp fov near far) = (4><4) [
    1/(asp*tan(fov/2)),          0.0,  0.0, 0.0
  ,                0.0, 1/tan(fov/2),  0.0, 0.0
  ,                0.0,          0.0, (near+far)/(near-far), 2*far*near/(near-far)
  ,                0.0,          0.0, -1.0, 0.0
  ]

frogZero :: FrogVector
frogZero = fromList [0, 0, 0]

frogUp :: FrogVector
frogUp = fromList [0, 1, 0]

frogLookAt :: FrogVector -> FrogVector -> FrogMatrix
frogLookAt eye target =
  let dir = normalize (target-eye)
      right = normalize (cross frogUp dir)
      up = cross dir right
  in (fromRows [right, up, dir, fromList (replicate 3 0)] ||| col [0, 0, 0, 1])
      * fromTranslation (toList -eye)

row :: Element t => [t] -> Matrix t
row = asRow.fromList

col :: Element t => [t] -> Matrix t
col = asColumn.fromList

normalize :: (Fractional t, Element t) => Vector t -> Vector t
normalize v =
  let l = toList v
      d = sum $ map (^(2::Integer)) l
  in fromList (map (/d) l)
