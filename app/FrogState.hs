{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FrogState where

import Control.Lens (makeLenses)
import Control.Monad.State (StateT, MonadTrans (lift))

import Key
import Time
import Matrix (RenderView)


type News = (KeySet, RenderView, Time)

data StateName = Play | Pause | Menu deriving (Show, Eq, Ord)

data Settings = Settings {
  _isShowingTicks :: Bool
, _isShowingKeys :: Bool
, _isRunningTests :: Bool
} deriving (Show, Eq)
makeLenses ''Settings

makeSettings :: Settings
makeSettings = Settings {
  _isShowingTicks = False
, _isShowingKeys = False
, _isRunningTests = False
}

class Stately a where
  _name :: a -> StateName
  _update :: News -> StateT a IO ()

preent :: Show a => a -> StateT b IO ()
preent = lift . print
