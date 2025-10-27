{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module World where

import SDL
import Foreign.C
import Control.Lens

import Mean

zfully :: Num a => V2 a -> V3 a
zfully z = V3 (z^._x) (z^._y) 0

zlessly :: Num a => V3 a -> V2 a
zlessly z = V2 (z^._x) (z^._y)

world :: V3 CFloat
world = fromIntegral <$> zfully (windowInitialSize defaultWindow)

center :: V3 CFloat
center = world ^/ 2

safeRect :: RealFrac a => V2 a -> V2 a -> Maybe (Rectangle CInt)
safeRect topLeft size = Just $ Rectangle (P $ round <$> topLeft) (round <$> size)