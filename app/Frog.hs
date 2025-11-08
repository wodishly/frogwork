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
import Mean (given)
import Time (Time, delta)


data Frogwit = Frogwit {
    _position :: Point3
  , _dy :: GLfloat
  , _speed :: GLfloat
  , _aLeap :: GLfloat
  , _weight :: GLfloat
} deriving (Show, Eq)
makeLenses ''Frogwit

makeFrog :: Frogwit
makeFrog = Frogwit {
    _position = Vertex3 0 0 0
  , _dy = 0
  , _speed = 4
  , _aLeap = 4
  , _weight = -8
}

leap :: KeySet -> StateT Frogwit IO Bool
leap keys = do
  frogwit <- get
  if keyBegun keys ScancodeSpace
    then put frogwit { _dy = frogwit^.aLeap } >> return True
    else return False

fall :: Time -> StateT Frogwit IO Bool
fall time = do
  frogwit <- get
  let Vertex3 x y z = frogwit^.position
      y' = max 0 (y + throttle time (frogwit^.dy))
      dy' = frogwit^.dy + throttle time (frogwit^.weight)
  put frogwit {
      _dy = given (== 0) y' dy'
    , _position = Vertex3 x y' z
  }
  return (frogwit^.dy /= 0)

throttle :: Time -> GLfloat -> GLfloat
throttle time = (*) (fromIntegral (time^.delta) / 1000)

walk :: KeySet -> Time -> FrogVector -> StateT Frogwit IO Bool
walk keys time forward = do
  frogwit <- get
  let direction = hat3 $ Vertex3 (forward!0) 0 -(forward!2)
      didWalk = dz < 0 where Vertex2 _ dz = wasd keys
      position' = liftA2 (+)
        ((* (throttle time $ frogwit^.speed)) <$> direction)
        (frogwit^.position)

  if didWalk
    then put frogwit { _position = position' } >> return True
    else return False

moveFrog :: KeySet -> Time -> FrogVector -> StateT Frogwit IO Bool
moveFrog keys time forward = do
  or <$> sequence [walk keys time forward, leap keys, fall time]
