module PauseState where

import Blee
import Mean
import Rime
import State
import Stavework


newtype PauseState = PauseState {
  _writings :: [Writing]
}
makeLenses ''PauseState

instance Stately PauseState where
  name _ = PauseName

  render allwit = do
    bg black
    stave writings allwit
    return allwit

makePauseState :: Point -> PauseState
makePauseState window = PauseState {
  _writings = [makeWriting ((1/2) *^ window) "pɔz"]
  }
