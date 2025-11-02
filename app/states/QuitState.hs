module QuitState where

import State
import Light

quitState :: GameState
quitState _ctx _keys _events stateInfo = do
  bg black
  return stateInfo