module Skeleton where

import Data.Maybe (fromMaybe)
import Numeric.LinearAlgebra (flatten, toList)

import Graphics.Rendering.OpenGL as GL

import Matrix (FrogMatrix, toFrogList, FrogVertex (toFrogVector), fromAffine)
import MothSpell as MOTH
import Data.List (findIndex)
import Rime (clamp, (<->), (<+>), Point4, (^/), Point3)
import Numeric.LinearAlgebra.HMatrix ( dot, (><) )
import Data.Fixed (mod')

data Animation = Animation {
  aMoth :: MothFile
, aTime :: Float
}

collectively :: Point3 -> Point4 -> Point3 -> FrogMatrix
collectively position3d (Vertex4 x y z s) scale3d =
  fromAffine (toFrogList scale3d) (toFrogList position3d) <>
  (4><4) [
    1 - 2*y**2 - 2*z**2, 2*x*y - 2*s*z, 2*x*z + 2*s*y, 0
  , 2*x*y + 2*s*z, 1 - 2*x**2 - 2*z**2, 2*y*z - 2*s*x, 0
  , 2*x*z - 2*s*y, 2*y*z + 2*s*x, 1 - 2*x**2 - 2*y**2, 0
  , 0, 0, 0, 1
  ]

worldify :: Maybe FrogMatrix -> [FrogMatrix] -> [MothBone] -> Int -> FrogMatrix
worldify world localMatrices bones boneIndex = do
  let parentIndex = fromIntegral (mother $ bones !! boneIndex)
      localMatrix = localMatrices !! boneIndex
      worldMatrix = fromMaybe (MOTH.matrix (bones !! boneIndex)) world

  case parentIndex of
    -- root bone
    255 -> localMatrix <> worldMatrix
    -- non-root
    _ -> worldify (Just $ localMatrix <> worldMatrix) localMatrices bones parentIndex

frame :: [a] -> [GLfloat] -> Float -> (a, a, Float)
frame values times now =
  let next = fromMaybe (length times - 1) (findIndex (> now) times)
      current = if next == 0 then 0 else next - 1
      nextT = times !! next
      currentT = times !! current
      nextV = values !! next
      currentV = values !! current
      t = clamp (0, 1) $ (now - currentT) / (nextT - currentT)
  in (currentV, nextV, t)

type Interpolation a = a -> a -> Float -> a

{-# INLINE (*^) #-}
(*^) :: (Functor f, Num a) => a -> f a -> f a
(*^) a = fmap (a*)

swizzle :: Vertex4 a -> Vertex4 a
swizzle v = let Vertex4 y z real x = v in Vertex4 x y z real

slerp :: Point4 -> Point4 -> Float -> Point4
slerp (Vertex4 qw qx qy qz) (Vertex4 pw px py pz) t
  | 1.0 - cosphi < 1e-8 = swizzle q
  | otherwise           = swizzle $ ((sin ((1-t)*phi) *^ q) <+> sin (t*phi) *^ f p) ^/ sin phi
  where
    q = Vertex4 qx qy qz qw
    p = Vertex4 px py pz pw
    dqp = dot (toFrogVector q) (toFrogVector p)
    (cosphi, f) = if dqp < 0 then (-dqp, (-1 *^)) else (dqp, id)
    phi = acos cosphi

vlerp :: (Applicative a, Num b) => a b -> a b -> b -> a b
vlerp x y t = x <+> (t *^ (y <-> x))

local :: Functor a => MothFile -> Float -> Interpolation (a GLfloat) -> (MothTale -> ([a GLfloat], [GLfloat])) -> [a GLfloat]
local mammoth now interpolate prop = map (\mothtale ->
        let (values, times) = prop mothtale
            (currentV, nextV, t) = frame values times now
        in interpolate currentV nextV t
       ) $ chronicles mammoth

m3 :: (MothTale -> Mothly3) -> (MothTale -> ([Vertex3 GLfloat], [GLfloat]))
m3 prop t = let Mothly3 values times = prop t in (values, times)
m4 :: (MothTale -> Mothly4) -> (MothTale -> ([Vertex4 GLfloat], [GLfloat]))
m4 prop t = let Mothly4 values times = prop t in (values, times)

play :: Animation -> Float -> IO [GLfloat]
play animoth now' = do
  let mammoth = aMoth animoth
      fossil = skeleton mammoth
      now = mod' (now' - aTime animoth) 1.0 -- 1.0 is duration
      confused = local mammoth now vlerp $ m3 MOTH.position
      confounded = local mammoth now slerp $ m4 MOTH.quaternion
      dazed = local mammoth now vlerp $ m3  MOTH.scale

  let be = zipWith3
      bonewards = be collectively confused confounded dazed
      ma'ammoth = map (worldify Nothing bonewards fossil) [0..(length fossil - 1)]

  return $ concatMap (toList . flatten) ma'ammoth
