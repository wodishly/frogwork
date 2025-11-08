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
import Graphics.Rendering.OpenGL (Vertex2 (Vertex2), GLfloat, Vertex3 (Vertex3))

import Key (KeySet, wasd, keyBegun)
import Matrix (FrogVector, Point3, hat3)
import Time (Time, delta)
import Mean (given)


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
  , _speed = 5
  , _aLeap = 2
  , _weight = -0.2
}

leap :: KeySet -> StateT Frogwit IO Bool
leap keys = do
  frogwit <- get
  if keyBegun keys ScancodeSpace
    then put frogwit { _dy = frogwit^.aLeap } >> return True
    else return False

fall :: StateT Frogwit IO ()
fall = do
  frogwit <- get
  let Vertex3 x y z = frogwit^.position
      y' = max 0 (y+frogwit^.dy)
  put frogwit {
      _dy = given (== 0) y' (frogwit^.dy + frogwit^.weight)
    , _position = Vertex3 x y' z
  }

throttle :: Time -> GLfloat -> GLfloat
throttle time = (*) (fromIntegral (time^.delta) / 1000)

walk :: KeySet -> Time -> FrogVector -> StateT Frogwit IO Bool
walk keys time forward = do
  frogwit <- get
  let direction = hat3 $ Vertex3 (forward!0) 0 -(forward!2)
      didWalk = let Vertex2 _ dz = wasd keys in dz < 0
      position' = liftA2 (+)
        ((* throttle time (frogwit^.speed)) <$> direction)
        (frogwit^.position)

  if didWalk
    then put frogwit { _position = position' } >> return True
    else return False

moveFrog :: KeySet -> Time -> FrogVector -> StateT Frogwit IO Bool
moveFrog keys time forward = do
  didMove <- (||) <$> walk keys time forward <*> leap keys
  fall
  return didMove
