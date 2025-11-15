module PauseState (
  PauseState (..)
, makePauseState
) where

import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (PauseName), Stately (..))

import Blee (bg, black)
import Matrix (RenderView (size))
import Stavework (stavewrite, renderFeather, Writing, makeWriting)
import Control.Monad.State (MonadState(get))
import Rime ((*^))


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

makePauseState :: RenderView -> PauseState
makePauseState dis = PauseState {
  writings = [
    makeWriting "pɔz" ((1/2) *^ Vertex2 width height)
  ]
} where (width, height) = size dis