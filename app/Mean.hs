{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Mean where

import SDL hiding (trace)
import Foreign.C
import Debug.Trace
import Data.Bifunctor
import System.Random

type Seed = [CFloat]
type Rand = CFloat

ly' :: Show b => (a -> b) -> a -> a
ly' f x = trace (show (f x)) x

ly :: Show a => a -> a
ly = ly' id

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

lif :: a -> a -> Bool -> a
lif q p condition = if condition then p else q