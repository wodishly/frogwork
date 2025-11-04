{-# LANGUAGE TemplateHaskell #-}

module PauseState where

import Control.Lens
import Control.Monad.State

import FrogState
import Light

data PauseState = PauseState {
}
makeLenses ''PauseState

type PauseUpdate = News -> StateT PauseState IO ()

instance Stately PauseState where
  _name _ = Pause

makePauseState :: PauseState
makePauseState = PauseState {
}

pauseState :: News -> StateT PauseState IO ()
pauseState _ = do
  lift $ bg blue