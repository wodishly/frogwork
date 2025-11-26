module PauseState where

import Control.Lens
import Control.Monad.State

import State

import Blee
import Rime
import Stavework 


newtype PauseState = PauseState {
  _writings :: [Writing]
}
makeLenses ''PauseState

instance Stately PauseState where
  name _ = PauseName

  render allwit = do
    pausewit <- get
    bg black
    renderFeather allwit
    ws <- stavewriteAll allwit (_writings pausewit)
    put pausewit { _writings = ws }
    return allwit

makePauseState :: Point -> PauseState
makePauseState window = PauseState { _writings = [makeWriting ((1/2) *^ window) "pɔz"] }
