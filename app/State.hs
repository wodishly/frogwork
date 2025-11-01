{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module State where

import Control.Lens (makeLenses, Lens', set, (^.))

import SDL.Event (Event)
import SDL.Video.OpenGL (GLContext)
import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL as GL

import Key
import Random
import Light
import Frog
import Rime
import Time

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

data StateInfo = StateInfo {
  _seed :: Seed
, _currentState :: StateName
, _states :: [StateName]
, _time :: Time
, _options :: OptionsInfo
, _frog :: Polygon
, _lily :: Point
-- locations could be moved to their own data type?
, _uloc :: UniformLocation
, _tloc :: UniformLocation
, _uvloc :: UniformLocation
, _keyset :: KeySet
, _menuFinger :: Int
--, _feather :: Feather
}
makeLenses ''StateInfo

defaultState :: StateInfo
defaultState = StateInfo {
  _seed = defaultSeed
, _currentState = Menu
, _states = [Play, Pause, Menu, Quit]
, _time = startTime
, _options = defaultOptions
, _frog = makeFrog
, _lily = Vertex2 0 0
, _uloc = UniformLocation 0
, _tloc = UniformLocation 0
, _uvloc = UniformLocation 0
, _keyset = unkeys
, _menuFinger = 0
--, _feather = defaultFeather
}

type Response = [Event] -> StateInfo -> IO StateInfo
type GameState = GLContext -> KeySet -> Response

understand :: KeySet -> Response
understand keys _events stateInfo = do
  stateInfo <- pure $ set keyset keys stateInfo
  stateInfo <- toggleOption ScancodeK isShowingKeys stateInfo
  stateInfo <- toggleOption ScancodeT isShowingTicks stateInfo
  decideState stateInfo

decideState :: StateInfo -> IO StateInfo
decideState stateInfo = case stateInfo^.currentState of
  Menu -> do
    stateInfo <- pure $ set menuFinger (navigate stateInfo (stateInfo^.menuFinger) 3) stateInfo
    return $ if (mod (stateInfo^.menuFinger) 3 /= 2) && keyBegun (stateInfo^.keyset) ScancodeReturn
      then set currentState Play stateInfo
      else stateInfo
  _ -> do
    return $ if keyBegun (stateInfo^.keyset) ScancodeP
      then togglePause stateInfo
      else stateInfo

navigate :: StateInfo -> Int -> Int -> Int
navigate stateInfo finger = mod (finger + cast (wayUpDown (stateInfo^.keyset) keyBegun))

togglePause :: StateInfo -> StateInfo
togglePause stateInfo = set currentState (case stateInfo^.currentState of
    Play -> Pause
    Pause -> Play
    _ -> error "bad state"
  ) stateInfo

toggleOption :: Scancode -> Lens' OptionsInfo Bool -> StateInfo -> IO StateInfo
toggleOption keycode lens stateInfo = pure $ if keyBegun (stateInfo^.keyset) keycode
  then set (options.lens) (not $ stateInfo^.options.lens) stateInfo
  else stateInfo