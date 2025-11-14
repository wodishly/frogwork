module PauseState (
  PauseState (..)
, makePauseState
) where

import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (PauseName), Stately (..))

import Blee (bg, black, white)
import Matrix (RenderView (size))
import Stavework (stavewrite, renderFeather)
import Stavemake (Staveware)
import Control.Monad.State (MonadState(get))
import Rime ((*^))


newtype PauseState = PauseState Staveware

instance Stately PauseState where
  name _ = PauseName
  staveware (PauseState ware) = ware

  update _ = return ()
  render (_, _, display, time) = do
    statewit <- get
    bg black
    renderFeather display time (staveware statewit)
    let (width, height) = size display
    stavewrite display ((1/2) *^ Vertex2 width height) white "pɔz"

makePauseState :: Staveware -> PauseState
makePauseState = PauseState
