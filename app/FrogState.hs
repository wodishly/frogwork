{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FrogState where

import Control.Lens

import qualified SDL.Video.OpenGL as SDL
import SDL.Input.Keyboard.Codes

import Graphics.Rendering.OpenGL as GL

import Key
import Time
import Control.Monad.State (StateT)
import Shade
import SDL (Window)
import Matrix (FrogVector, frogZero)

type News = (KeySet, Window, Time)

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
, _isShowingKeys = True
, _isRunningTests = False
}

class Stately a where
  _name :: a -> StateName
  _update :: News -> StateT a IO ()
