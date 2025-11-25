module PauseState (
  PauseState (..),
  makePauseState,
  writings
) where

import Control.Lens (makeLenses)
import Control.Monad.State (MonadState (get, put))

import State (StateName (PauseName), Stately (..))

import Blee (bg, black)
import Rime (Point, (*^))
import Stavework (Writing, makeWriting, renderFeather, stavewriteAll)


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
makePauseState wind = PauseState { _writings = [makeWriting ((1/2) *^ wind) "pɔz"] }
