module PauseState (
  PauseState (..)
, makePauseState
) where

import Control.Lens (makeLenses)

import State (StateName (PauseName), Stately (..))

import Blee (bg, blue)


data PauseState = PauseState
makeLenses ''PauseState

instance Stately PauseState where
  name _ = PauseName
  update _ = return ()
  render _ = bg blue

makePauseState :: PauseState
makePauseState = PauseState
