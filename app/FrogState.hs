{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FrogState where

import Control.Lens

import SDL (Event, Window)

import Key
import Time

type News = ([Event], KeySet, Window, Time)

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
, _isShowingKeys = False
, _isRunningTests = False
}

class Stately a where
  _name :: a -> StateName
