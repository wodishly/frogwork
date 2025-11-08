module Rime (
  average
, clamp
, hardRound
) where

import Graphics.Rendering.OpenGL (GLfloat)
import Mean (doBoth)


average :: Real a => [a] -> GLfloat
average = uncurry (/) . doBoth (realToFrac.sum) (fromIntegral.length)

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) = min high . max low

-- | Rounds to type @Int@ rather than to an inferred @Integral a@.
hardRound :: RealFrac a => a -> Int
hardRound x = round x :: Int
