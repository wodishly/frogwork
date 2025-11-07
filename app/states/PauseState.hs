module PauseState (
  PauseState (..)
, makePauseState
) where

import Control.Lens (makeLenses)
import Control.Monad.State (StateT)

import FrogState (Stately (..), News, StateName (..))

import Light (blue, bg)

data PauseState = PauseState
makeLenses ''PauseState

instance Stately PauseState where
  _name _ = PauseName
  _update = pause

makePauseState :: PauseState
makePauseState = PauseState

pause :: News -> StateT PauseState IO ()
pause _ = do
  bg blue
