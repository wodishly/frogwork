{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Mean where

import SDL hiding (trace)
import Data.Bifunctor
import Debug.Trace
import Foreign.C
import System.Random
import Data.Function

type Pair a = (a, a)

ly' :: Show b => (a -> b) -> a -> a
ly' f x = trace (show (f x)) x

ly :: Show a => a -> a
ly = ly' id

weep :: IO ()
weep = print "wah"

average :: (Fractional a) => [a] -> a
average = uncurry (/) . bothly sum (fromIntegral.length)

bothly :: (a -> b) -> (a -> c) -> a -> (b, c)
bothly f g = bimap f g . twin

twin :: a -> (a, a)
twin x = (x, x)

triple :: a -> (a, a, a)
triple x = (x, x, x)

-- given a dyadic function `f`, applies `x` to both arguments
untwin :: (a -> a -> b) -> a -> b
untwin f x = f x x
-- applies a function to both arguments of a pair
twimap :: (a -> b) -> (a, a) -> (b, b)
twimap = untwin bimap

-- given a dyadic function `f`, applies `x` to both arguments
untriple :: (a -> a -> a -> b) -> a -> b
untriple f x = f x x x

-- applies a function to all three arguments of a pair
thrimap :: (a -> b) -> (a, a, a) -> (b, b, b)
thrimap = untriple trimap

-- like bimap but for three things
trimap :: (a -> b) -> (c -> d) -> (e -> f) -> (a, c, e) -> (b, d, f)
trimap f g h (x,y,z) = (f x, g y, h z)

cast :: (Enum a, Num b) => a -> b
cast = fromIntegral.fromEnum

lif :: Bool -> a -> a -> a
lif condition p q = if condition then p else q

-- whether `x` gladdens all in `fs`
allIn :: (Foldable t) => t (a -> Bool) -> a -> Bool
allIn fs x = all ($ x) fs

-- whether `x` gladdens any in `fs`
anyIn :: (Foldable t) => t (a -> Bool) -> a -> Bool
anyIn fs x = any ($ x) fs

flight :: Int -> [Int]
flight = enumFromTo 0 . pred