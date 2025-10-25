{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module World where

import SDL
import Foreign.C
import Control.Lens

import Mean

world :: V2 CInt
world = windowInitialSize defaultWindow

center :: V2 CFloat
center = fmap ((/ 2) . fromIntegral) world

nought :: V2 CFloat
nought = V2 0 0

x :: V2 CFloat -> CFloat
x = view _x

y :: V2 CFloat -> CFloat
y = view _y

-- rx :: Rand -> CFloat
-- rx r = (x $ cast world) * r

rway :: IO (V2 CFloat)
rway = (uncurry V2) <$> (bimap cos sin) <$> (twin <$> rangle)

twain :: V2 CFloat -> (CFloat, CFloat)
twain z = (x z, y z)

worth :: V2 CFloat -> CFloat
worth = sqrt . uncurry (+) . twimap (^2) . twain

stretch :: CFloat -> V2 CFloat -> V2 CFloat
stretch r = fmap (*r)

onehood :: V2 CFloat -> V2 CFloat
onehood z = case worth z of
  0 -> nought
  _ -> stretch (1 / worth z) z

type Keysuch = Suchness Scancode

wayward :: Keysuch -> V2 CFloat
wayward ks = onehood $ V2
  ((cast.ks) ScancodeRight - (cast.ks) ScancodeLeft)
  ((cast.ks) ScancodeDown  - (cast.ks) ScancodeUp  )

class Wending a where
  wend :: a -> a

class Wending a => Stirring a where
  z, dz :: a -> V2 CFloat

data Athem = Athem Glee Might
instance Wending Athem where
  wend (Athem g m) = Athem (wend g) (wend m)

--athem :: Athem
--athem = Athem glee might

data Glee = Glee {
  size :: V2 CFloat
, wealth :: Int
}

instance Wending Glee where
  wend g = Glee (size g) (succ $ wealth g)

glee :: Glee
glee = Glee (V2 64 64) 0

data Might = Might (V2 CFloat) (V2 CFloat)

instance Wending Might where
  wend (Might z dz) = Might (z+dz) (dz)

-- rmight :: Rand -> Rand -> Rand -> Might
-- rmight r0 r1 r2 = Might (V2 (rx r0) (ry r1)) rway