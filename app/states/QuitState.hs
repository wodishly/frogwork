module QuitState where

import FrogState
import Light

quitState :: GameState
quitState _ctx _keys stateInfo = do
  bg black
  return stateInfo