module Skeleton where

import Data.Fixed (mod')
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

import Numeric.LinearAlgebra (dot, flatten, toList, (><))
import Graphics.Rendering.OpenGL as GL

import Matrix (FrogMatrix, fromAffine)
import Rime (
    FrogVertex (toFrogVector)
  , Point3
  , Point4
  , clamp
  , toFrogList
  , (<+>)
  , (<->)
  , (*^)
  , (^/)
  )

import MothSpell as MOTH
import Time (doCurrentTime)
import Mean (flight)

type Interpolation a = a -> a -> Float -> a

data Animation = Animation {
  aMoth :: MothFile
, aTime :: Float
, tomeIndex :: Int
, playing :: Bool
, looped :: Bool
} deriving (Show)

makeAnimation :: MothFile -> IO Animation
makeAnimation mothFile = do
  now <- doCurrentTime
  return Animation { aMoth = mothFile, aTime = now, tomeIndex = -1, playing = False, looped = False }

play :: Animation -> Int -> IO Animation
play a clip = do
  t <- if not (playing a) || clip /= tomeIndex a then doCurrentTime else return $ aTime a
  return a { playing = True, tomeIndex = clip, aTime = t }

once :: Animation -> Animation
once a = a { looped = False }

evermore :: Animation -> Animation
evermore a = a { looped = True }

collectively :: Point3 -> Point4 -> Point3 -> FrogMatrix
collectively position3d (Vertex4 x y z s) scale3d =
  fromAffine (toFrogList scale3d) (toFrogList position3d) <>
  (4><4) [
    1 - 2*y**2 - 2*z**2,       2*x*y - 2*s*z,       2*x*z + 2*s*y, 0
  ,       2*x*y + 2*s*z, 1 - 2*x**2 - 2*z**2,       2*y*z - 2*s*x, 0
  ,       2*x*z - 2*s*y,       2*y*z + 2*s*x, 1 - 2*x**2 - 2*y**2, 0
  ,                   0,                   0,                   0, 1
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

{-# INLINE swizzle #-}
swizzle :: Vertex4 a -> Vertex4 a
swizzle (Vertex4 y z real x) = Vertex4 x y z real

{-# INLINE unswizzle #-}
unswizzle :: Vertex4 a -> Vertex4 a
unswizzle (Vertex4 x y z real) = Vertex4 y z real x

slerp :: Point4 -> Point4 -> Float -> Point4
slerp q' p' t
  | 1.0 - cosphi < 1e-8 = swizzle q
  | otherwise           = swizzle $ ((sin ((1-t)*phi) *^ q) <+> sin (t*phi) *^ f p) ^/ sin phi
  where
    q = unswizzle q'
    p = unswizzle p'
    dqp = dot (toFrogVector q) (toFrogVector p)
    (cosphi, f) = if dqp < 0 then (-dqp, (-1 *^)) else (dqp, id)
    phi = acos cosphi

vlerp :: Applicative a => Interpolation (a Float)
vlerp x y t = x <+> (t *^ (y <-> x))

local :: FrogVertex v => MothTome -> Float -> Interpolation v -> (MothTale -> ([v], [GLfloat])) -> [v]
local mammoth now interpolate prop = map (\mothtale ->
        let (values, times) = prop mothtale
            (currentV, nextV, t) = frame values times now
        in interpolate currentV nextV t
       ) $ chronicles mammoth

m3 :: (MothTale -> Mothly3) -> MothTale -> ([Point3], [GLfloat])
m3 prop t = let Mothly3 values times = prop t in (values, times)

m4 :: (MothTale -> Mothly4) -> MothTale -> ([Point4], [GLfloat])
m4 prop t = let Mothly4 values times = prop t in (values, times)

continue :: Animation -> Float -> IO ([GLfloat], Bool)
continue animoth now' = do
  let moth = aMoth animoth
      mammoth = library moth !! tomeIndex animoth
      fossil = skeleton moth
      time = now' - aTime animoth
      forever = looped animoth
      now = if forever
        then mod' time (MOTH.duration mammoth)
        else clamp (0, MOTH.duration mammoth) time
      finished = not forever && (time >= MOTH.duration mammoth)
      confused = local mammoth now vlerp $ m3 MOTH.position
      confounded = local mammoth now slerp $ m4 MOTH.quaternion
      dazed = local mammoth now vlerp $ m3 MOTH.scale

  let be = zipWith3
      bonewards = be collectively confused confounded dazed
      ma'ammoth = map (worldify Nothing bonewards fossil) (flight $ length fossil)

  return (concatMap (toList . flatten) ma'ammoth, finished)
