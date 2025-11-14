module Frog (
  Frogwit (..)
, makeFrog
, moveFrog
) where

import Control.Monad.State (MonadState (get, put), StateT)
import Numeric.LinearAlgebra ((!))

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3))

import Key (Keyset, keyBegun, wasd)
import Matrix (FrogVector, hat)
import Mean (doBoth)
import Rime (Point3, (*^), (<+>))
import State (News)
import Time (Time, throttle)


data Frogwit = Frogwit {
    position :: Point3
  , dy :: GLfloat
  , speed :: GLfloat
  , aLeap :: GLfloat
  , weight :: GLfloat
  , leapCount :: Int
  , utleaps :: Int
} deriving (Show, Eq)

makeFrog :: Frogwit
makeFrog = Frogwit {
    position = Vertex3 0 0 0
  , dy = 0
  , speed = 2
  , aLeap = 5
  , weight = -8
  , leapCount = 0
  , utleaps = 2
}

hasLeapsLeft :: Frogwit -> Bool
hasLeapsLeft = uncurry (<) . doBoth leapCount utleaps

leap :: Keyset -> StateT Frogwit IO Bool
leap keys = do
  frogwit <- get
  if keyBegun keys ScancodeSpace && hasLeapsLeft frogwit
    then do
      put frogwit {
          dy = aLeap frogwit
        , leapCount = succ $ leapCount frogwit
      }
      return True
    else return False

fall :: Time -> StateT Frogwit IO Bool
fall time = do
  frogwit <- get
  let Vertex3 x y z = position frogwit
      y' = max 0 (y + throttle time (dy frogwit))
      dy' = dy frogwit + throttle time (weight frogwit)
  let landed = y' == 0
  put frogwit {
      dy = if landed then 0 else dy'
    , leapCount = if landed then 0 else leapCount frogwit
    , position = Vertex3 x y' z
  }
  return (dy frogwit /= 0)

walk :: News -> FrogVector -> StateT Frogwit IO Bool
walk (keys, _, _, time) forward = do
  frogwit <- get
  let direction = hat $ Vertex3 (forward!0) 0 -(forward!2)
      didWalk = dz < 0 where Vertex2 _ dz = wasd keys
      position' = position frogwit <+> (throttle time (speed frogwit) *^ direction)

  if didWalk
    then put frogwit { position = position' } >> return True
    else return False

moveFrog :: News -> FrogVector -> StateT Frogwit IO (Bool, Bool)
moveFrog news@(keys, _, _, time) forward = do
  didJump <- leap keys
  didFall <- fall time
  didMove <- or <$> sequence [walk news forward, return didJump, return didFall]
  return (didMove, didJump || didFall)
