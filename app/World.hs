{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module World where

import SDL
import Foreign.C
import Control.Lens

import Mean

lift :: Num a => V2 a -> V3 a
lift z = V3 (view _x z) (view _y z) 0

lower :: Num a => V3 a -> V2 a
lower z = V2 (view _x z) (view _y z)

world :: V3 CFloat
world = fromIntegral <$> lift (windowInitialSize defaultWindow)

center :: V3 CFloat
center = world ^/ 2

data Frog = Frog {
  size :: V3 CFloat
, wealth :: Int
}

frog :: Frog
frog = Frog (V3 64 64 64) 0