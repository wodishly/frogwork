module FrogState (
  News
, Settings (..)
, StateName (..)
, Stately (..)
, isRunningTests
, isShowingKeys
, isShowingTicks
, makeSettings
, preent
) where

import Control.Lens (makeLenses)
import Control.Monad.State (MonadTrans (lift), StateT)

import Key (KeySet)
import Matrix (RenderView)
import Time (Time)


type News = (KeySet, RenderView, Time)

data StateName = PlayName | PauseName | MenuName deriving (Show, Eq, Ord)

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
