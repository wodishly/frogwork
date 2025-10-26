module Random where

import Mean
import Foreign.C
import SDL
import System.Random
import Data.Bifunctor
import World
import Control.Lens

defaultSeed :: Seed
defaultSeed = randoms $ mkStdGen 0

-- returns a random number in [0, 1)
rand :: IO CFloat
rand = randomIO

-- returns a random angle in [0, 2π)
rangle :: IO CFloat
rangle = fmap ((2 * pi) *) rand

rdir :: IO (V2 CFloat)
rdir = (uncurry V2 . bimap cos sin) . twin <$> rangle

rx :: Rand -> CFloat
rx = (*) (view _x $ cast <$> world)

ry :: Rand -> CFloat
ry = (*) (view _y $ cast <$> world)

rz :: Rand -> CFloat
rz = (*) (view _z $ cast <$> world)