module Frog (
  Frogwit (..)
, makeFrog
, moveFrog
) where

import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), StateT)

import Numeric.LinearAlgebra ((!))
import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3))

import Key (Keyset, keyBegun, wasd)
import Mean (ssss)
import Rime (FrogVector, Point3, hat, (*^), (<+>), FrogVertex ((^*^)))
import Time (Timewit, throttle)
import Allwit (Allwit (..))


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
hasLeapsLeft = ssss ((<) . leapCount) utleaps

leap :: Keyset -> StateT Frogwit IO Bool
leap keys = do
  frogwit <- get
  let didLeap = keyBegun keys ScancodeSpace && hasLeapsLeft frogwit
  when didLeap $ put frogwit {
      dy = aLeap frogwit
    , leapCount = succ $ leapCount frogwit
  }
  return didLeap

fall :: Timewit -> StateT Frogwit IO Bool
fall time = do
  frogwit <- get
  let Vertex3 x y z = position frogwit
      y' = y + throttle time (dy frogwit)
  if y' <= 0
    then land
    else do
      put frogwit {
        dy = dy frogwit + throttle time (weight frogwit)
      , position = Vertex3 x y' z
      }
      return True

land :: StateT Frogwit IO Bool
land = do
  frogwit <- get
  put frogwit {
    dy = 0
  , leapCount = 0
  , position = Vertex3 1 0 1 ^*^ position frogwit
  }
  return False

walk :: Allwit -> FrogVector -> StateT Frogwit IO Bool
walk allwit forward = do
  frogwit <- get
  let direction = hat $ Vertex3 (forward!0) 0 -(forward!2)
      didWalk = dz < 0 where Vertex2 _ dz = wasd (keyset allwit)
      position' = position frogwit <+> (throttle (timewit allwit) (speed frogwit) *^ direction)

  when didWalk $ put frogwit { position = position' }
  return didWalk

moveFrog :: Allwit -> FrogVector -> StateT Frogwit IO (Bool, Bool)
moveFrog allwit forward = do
  didWalk <- walk allwit forward
  didLeapOrFall <- or <$> sequence [leap $ keyset allwit, fall $ timewit allwit]

  let didMove = didWalk || didLeapOrFall in
    return (didMove, didLeapOrFall)
