{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FrogState where

import Control.Lens (makeLenses)
import Control.Monad.State (StateT)

import SDL (Window)

import Key
import Time
import Matrix

type News = (KeySet, Window, Time)

data StateName = Play | Pause | Menu deriving (Show, Eq, Ord)

data Settings = Choosewit {
  _isShowingTicks :: Bool
, _isShowingKeys :: Bool
, _isRunningTests :: Bool
}
makeLenses ''Settings

makeSettings :: Settings
makeSettings = Choosewit {
  _isShowingTicks = False
, _isShowingKeys = True
, _isRunningTests = False
}

class Stately a where
  _name :: a -> StateName
  _update :: News -> StateT a IO ()

data Camera = Camera {
  cPosition :: FrogVector,
  cTarget :: FrogVector
}
