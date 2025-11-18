module PauseState (
  PauseState (..)
, makePauseState
) where

import Control.Monad.State (MonadState(get, put))

import State (StateName (PauseName), Stately (..))

import Blee (bg, black)
import Rime (Point, (*^))
import Stavework (Writing, makeWriting, renderFeather, stavewriteAll)


newtype PauseState = PauseState {
  writings :: [Writing]
}

instance Stately PauseState where
  name _ = PauseName

  render allwit = do
    pausewit <- get
    bg black
    renderFeather allwit
    ws <- stavewriteAll allwit (writings pausewit)
    put pausewit { writings = ws }

makePauseState :: Point -> PauseState
makePauseState wind = PauseState { writings = [makeWriting ((1/2) *^ wind) "pɔz"] }
