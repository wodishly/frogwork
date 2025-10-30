module Mean where

import Debug.Trace (trace)
import Data.Function ((&))
import Data.Bifunctor (Bifunctor(first, bimap))

type Shed a = [a] -> a
type Shell a = a -> [a]

ly' :: Show b => (a -> b) -> a -> a
-- ly' f x = trace (show (f x)) x
ly' = (. twin) . (uncurry trace .) . (first . (show .))

ly :: Show a => a -> a
ly = ly' id

weep :: IO ()
weep = print "wah"

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) x = min high (max low x)

average :: (Fractional a) => Shed a
average = uncurry (/) . applyBoth sum (fromIntegral.length)

applyBoth :: (a -> b) -> (a -> c) -> a -> (b, c)
applyBoth f g = bimap f g . twin

twin :: a -> (a, a)
twin x = (x, x)

-- given a dyadic function `f`, applies `x` to both arguments
untwin :: (a -> a -> b) -> a -> b
untwin f x = f x x

-- applies a function to both arguments of a pair
twimap :: (a -> b) -> (a, a) -> (b, b)
twimap = untwin bimap

cast :: (Enum a, Num b) => a -> b
cast = fromIntegral.fromEnum

-- whether `x` gladdens all in `fs`
allIn :: (Foldable t) => t (a -> Bool) -> a -> Bool
allIn = flip (.) (&) . flip all

-- whether `x` gladdens any in `fs`
anyIn :: (Foldable t) => t (a -> Bool) -> a -> Bool
anyIn fs x = any ($ x) fs

flight :: Shell Int
flight = enumFromTo 0 . pred