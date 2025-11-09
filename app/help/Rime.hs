module Rime (
  average
, cast
, clamp
) where

import Graphics.Rendering.OpenGL (GLfloat)
import Mean (doBoth)


average :: Real a => [a] -> GLfloat
average = uncurry (/) . doBoth (realToFrac.sum) (fromIntegral.length)

-- | Casts @Bool@ and most numeric types to the needed inferred type.
cast :: (Enum a, Num b) => a -> b
cast = fromIntegral . fromEnum

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) = min high . max low
