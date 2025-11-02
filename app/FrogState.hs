{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FrogState where

import Control.Lens

import SDL.Input.Keyboard.Codes

import Graphics.Rendering.OpenGL as GL

import Key
import Random
import Light
import Rime
import Shade
import SDL (GLContext, Event)

data OptionsInfo = OptionsInfo {
  _isShowingTicks :: Bool
, _isShowingKeys :: Bool
, _isRunningTests :: Bool
}
makeLenses ''OptionsInfo

defaultOptions :: OptionsInfo
defaultOptions = OptionsInfo {
  _isShowingTicks = False
, _isShowingKeys = False
, _isRunningTests = False
}

data StateName = Play | Pause | Menu | Quit deriving (Show, Eq)

data Statewit = Statewit {
  _seed :: Seed,
  _currentState :: StateName,
  _states :: [StateName],
  _meshes :: [Mesh],
  _lily :: Point,
  _menuFinger :: Int,
  _programs :: [(Program, VertexArrayObject)]
  --, _feather :: Feather
}
makeLenses ''Statewit

makeState :: Statewit
makeState = Statewit {
  _seed = defaultSeed,
  _currentState = Menu,
  _states = [Play, Pause, Menu, Quit],
  -- _time = beginTime,
  _meshes = [],
  _lily = Vertex2 0 0,
  _menuFinger = 0,
  _programs = []
  --, _feather = defaultFeather
}

type Response = [SDL.Event] -> Statewit -> IO Statewit
type GameState = SDL.GLContext -> KeySet -> Response

-- decideState :: Statewit -> IO Statewit
-- decideState stateInfo = case stateInfo^.currentState of
--   Menu -> do
--     stateInfo <- pure $ set menuFinger (navigate stateInfo (stateInfo^.menuFinger) 3) stateInfo
--     return $ if (mod (stateInfo^.menuFinger) 3 /= 2) && keyBegun (stateInfo^.keys) ScancodeReturn
--       then set currentState Play stateInfo
--       else stateInfo
--   _ -> do
--     return $ if keyBegun (stateInfo^.keys) ScancodeP
--       then togglePause stateInfo
--       else stateInfo
-- 
-- navigate :: Statewit -> Int -> Int -> Int
-- navigate stateInfo finger = mod (finger + cast (wayUpDown (stateInfo^.keys) keyBegun))
-- 