module Rime where
import Graphics.Rendering.OpenGL (GLfloat)

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) x = min high (max low x)

average :: Real a => [a] -> GLfloat
average xs = realToFrac (sum xs) / fromIntegral (length xs)

-- rounds specifically to type `Int` rather than some type `Integral a => a`
hardRound :: RealFrac a => a -> Int
hardRound x = round x :: Int


cast :: (Enum a, Num b) => a -> b
cast = fromIntegral.fromEnum
-- unused
roundTo :: (RealFrac a) => Int -> a -> Double
roundTo sharpness n = (fromInteger.round $ n*10^sharpness) / (10.0^sharpness)