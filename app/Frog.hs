module Frog (
  Frogwit (..)
, position
, dy
, weight
, makeFrog
, moveFrog
) where

import Control.Lens (makeLenses, (^.))
import Control.Monad.State (MonadState (get, put), StateT)
import Numeric.LinearAlgebra ((!))

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3))

import Key (KeySet, keyBegun, wasd)
import Matrix (FrogVector, Point3, hat3)
import Time (Time, throttle)
import Control.Monad (when)
import Mean (doBoth)
import State (News)


data Frogwit = Frogwit {
    _position :: Point3
  , _dy :: GLfloat
  , _speed :: GLfloat
  , _aLeap :: GLfloat
  , _weight :: GLfloat
  , _leapCount :: Int
  , _utleaps :: Int
} deriving (Show, Eq)
makeLenses ''Frogwit

makeFrog :: Frogwit
makeFrog = Frogwit {
    _position = Vertex3 0 0 0
  , _dy = 0
  , _speed = 2
  , _aLeap = 4
  , _weight = -8
  , _leapCount = 0
  , _utleaps = 2
}

hasLeapsLeft :: Frogwit -> Bool
hasLeapsLeft = uncurry (<) . doBoth (^.leapCount) (^.utleaps)

leap :: KeySet -> StateT Frogwit IO Bool
leap keys = do
  frogwit <- get
  if keyBegun keys ScancodeSpace && hasLeapsLeft frogwit
    then do
      put frogwit {
          _dy = frogwit^.aLeap
        , _leapCount = succ $ frogwit^.leapCount
      }
      return True
    else return False

fall :: Time -> StateT Frogwit IO Bool
fall time = do
  frogwit <- get
  let Vertex3 x y z = frogwit^.position
      y' = max 0 (y + throttle time (frogwit^.dy))
      dy' = frogwit^.dy + throttle time (frogwit^.weight)
  put frogwit {
      _dy = dy'
    , _position = Vertex3 x y' z
  }
  when (y' == 0) $ put frogwit { _leapCount = 0 }
  return (frogwit^.dy /= 0)

walk :: News -> FrogVector -> StateT Frogwit IO Bool
walk (keys, _, _, _, time) forward = do
  frogwit <- get
  let direction = hat3 $ Vertex3 (forward!0) 0 -(forward!2)
      didWalk = dz < 0 where Vertex2 _ dz = wasd keys
      position' = liftA2 (+)
        ((* (throttle time $ frogwit^.speed)) <$> direction)
        (frogwit^.position)

  if didWalk
    then put frogwit { _position = position' } >> return True
    else return False

moveFrog :: News -> FrogVector -> StateT Frogwit IO Bool
moveFrog news@(keys, _, _, _, time) forward = do
  or <$> sequence [walk news forward, leap keys, fall time]
