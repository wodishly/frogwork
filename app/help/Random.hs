module Random (
  FrogSeed
, defaultSeed
, rand
) where

import System.Random (randoms, mkStdGen, randomIO)

import Graphics.Rendering.OpenGL (GLfloat)


type FrogSeed = [GLfloat]

defaultSeed :: FrogSeed
defaultSeed = randoms $ mkStdGen 0

-- returns a random number in [0, 1)
rand :: IO GLfloat
rand = randomIO
