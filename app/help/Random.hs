module Random where

import System.Random

import Graphics.Rendering.OpenGL (GLfloat)


type FrogSeed = [GLfloat]

formseed :: FrogSeed
formseed = randoms (mkStdGen 0)

-- returns a random number in [0, 1)
rand :: IO GLfloat
rand = randomIO
