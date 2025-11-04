module PauseState where

import FrogState
import Light

pauseState :: GameState
pauseState _ctx _keys stateInfo = do
  bg blue
  return stateInfo