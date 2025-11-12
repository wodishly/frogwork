module PauseState (
  PauseState (..)
, makePauseState
) where

import State (StateName (PauseName), Stately (..))

import Blee (bg, blue)
import Stavemake (Staveware)


newtype PauseState = PauseState Staveware

instance Stately PauseState where
  name _ = PauseName
  update _ = return ()
  render _ = bg blue
  staveware (PauseState ware) = ware

makePauseState :: Staveware -> PauseState
makePauseState = PauseState
