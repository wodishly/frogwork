{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Mean where

import SDL hiding (trace)
import Foreign.C
import Debug.Trace
import Data.Bifunctor
import System.Random

type Suchness a = a -> Bool
type Seed = [CFloat]
type Rand = CFloat

ly' :: Show b => (a -> b) -> a -> a
ly' f x = trace (show (f x)) x

ly :: Show a => a -> a
ly = ly' id

twin :: a -> (a,a)
twin x = (x,x)

untwin :: (a -> a -> b) -> a -> b
untwin f x = f x x

twimap :: (a -> b) -> (a, a) -> (b, b)
twimap = untwin bimap

cast :: (Enum a, Num b) => a -> b
cast = fromIntegral.fromEnum

rand :: IO CFloat
rand = randomIO

rangle :: IO CFloat
rangle = liftA2 (*) (liftA2 (*) (pure 2) (pure pi)) rand