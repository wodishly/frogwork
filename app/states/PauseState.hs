module PauseState where

import FrogState
import Light

pauseState :: GameState
pauseState _ctx _keys _events stateInfo = do
  bg blue
  return stateInfo