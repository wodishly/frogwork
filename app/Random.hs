module Random where

import SDL
import Data.Bifunctor
import System.Random
import Control.Lens
import Foreign.C

import Mean
import World

type Seed = [CFloat]
type Rand = CFloat

defaultSeed :: Seed
defaultSeed = randoms $ mkStdGen 0

-- returns a random number in [0, 1)
rand :: IO CFloat
rand = randomIO

-- returns a random angle in [0, 2π)
rangle :: IO CFloat
rangle = fmap ((2 * pi) *) rand

-- random 2d unit vector
rdir :: IO (V2 CFloat)
rdir = (uncurry V2 . bimap cos sin) . twin <$> rangle

-- random x in [0, world.x)
rx :: IO CFloat
rx = fmap ((world^._x) *) rand

-- random y in [0, world.y)
ry :: IO CFloat
ry = fmap ((world^._y) *) rand

-- random z in [0, world.z)
rz :: IO CFloat
rz = fmap ((world^._z) *) rand