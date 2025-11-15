module PauseState (
  PauseState (..)
, makePauseState
) where

import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (PauseName), Stately (..))

import Blee (bg, black)
import Matrix (RenderView (size))
import Stavework (stavewrite, renderFeather, Writing, makeWriting)
import Stavemake (Staveware)
import Control.Monad.State (MonadState(get))
import Rime ((*^))


data PauseState = PauseState {
  _staveware :: Staveware
, writings :: [Writing]
}

instance Stately PauseState where
  name _ = PauseName
  staveware (PauseState ware _) = ware

  update _ = return ()
  render news@(_, _, display, time) = do
    pausewit <- get
    bg black
    renderFeather display time (staveware pausewit)
    stavewrite news (writings pausewit)

makePauseState :: RenderView -> Staveware -> PauseState
makePauseState dis ware = PauseState {
  _staveware = ware
, writings = [
    makeWriting "pɔz" ((1/2) *^ Vertex2 width height)
  ]
} where (width, height) = size dis