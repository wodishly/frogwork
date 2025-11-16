module PauseState (
  PauseState (..)
, makePauseState
) where

import Control.Monad.State (MonadState(get))

import State (StateName (PauseName), Stately (..))

import Blee (bg, black)
import Rime (Point, (*^))
import Stavework (Writing, makeWriting, renderFeather, stavewrite)


newtype PauseState = PauseState {
  writings :: [Writing]
}

instance Stately PauseState where
  name _ = PauseName

  render allwit = do
    pausewit <- get
    bg black
    renderFeather allwit
    stavewrite allwit (writings pausewit)

makePauseState :: Point -> PauseState
makePauseState wind = PauseState {
  writings = [
    makeWriting "pɔz" ((1/2) *^ wind)
  ]
}