module Random where

import System.Random (Random(randoms), mkStdGen, randomIO)
import Data.Bifunctor (Bifunctor(bimap))

import Graphics.Rendering.OpenGL (Vertex2(Vertex2), GLfloat)

import Light
import Mean

type Seed = [GLfloat]
type Rand = GLfloat

defaultSeed :: Seed
defaultSeed = randoms $ mkStdGen 0

-- returns a random number in [0, 1)
rand :: IO GLfloat
rand = randomIO

-- returns a random angle in [0, 2π)
rangle :: IO GLfloat
rangle = fmap ((2*pi) *) rand

-- random 2d unit vector
rdir :: IO Point
rdir = (uncurry Vertex2 . bimap cos sin) . twin <$> rangle

world :: Point
world = Vertex2 800 600

x :: Point -> GLfloat
x (Vertex2 _x _) = _x

y :: Point -> GLfloat
y (Vertex2 _ _y) = _y

-- random x in [0, world.x)
rx :: IO GLfloat
rx = fmap (x world *) rand

-- random y in [0, world.y)
ry :: IO GLfloat
ry = fmap (y world *) rand