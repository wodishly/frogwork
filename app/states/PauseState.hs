module PauseState (
  PauseState (..)
, makePauseState
) where

import State (StateName (PauseName), Stately (..))

import Blee (bg, blue)
import Stave (Staveware)


newtype PauseState = PauseState Staveware

instance Stately PauseState where
  name _ = PauseName
  update _ = return ()
  render _ = bg blue

makePauseState :: Staveware -> PauseState
makePauseState = PauseState
