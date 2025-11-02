module QuitState where

import FrogState
import Light

quitState :: GameState
quitState _ctx _keys _events stateInfo = do
  bg black
  return stateInfo