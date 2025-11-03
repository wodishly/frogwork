{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FrogState where

import Control.Lens

import qualified SDL.Event as SDL
import qualified SDL.Video.OpenGL as SDL
import SDL.Input.Keyboard.Codes

import Graphics.Rendering.OpenGL as GL

import Key
import Random
import Light
import Rime
import Time
import Shade
import SDL (Window)
import Matrix (FrogVector, frogZero)

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

data Camera = Camera {
  cPosition :: FrogVector,
  cTarget :: FrogVector
}

data StateWit = StateWit {
  _seed :: Seed
, _currentState :: StateName
, _states :: [StateName]
, _time :: Time
, _options :: OptionsInfo
, _meshes :: [Mesh]
, _lily :: Point
, _camera :: Camera
, _keyset :: KeySet
, _menuFinger :: Int
, _programs :: [(Program, VertexArrayObject)]
, _window :: Maybe Window
--, _feather :: Feather
}
makeLenses ''StateWit

defaultState :: StateWit
defaultState = StateWit {
  _seed = defaultSeed
, _currentState = Menu
, _states = [Play, Pause, Menu, Quit]
, _time = startTime
, _options = defaultOptions
, _meshes = []
, _camera = Camera { cPosition = frogZero, cTarget = [0, 0, 1] }
, _lily = Vertex2 0 0
, _keyset = unkeys
, _menuFinger = 0
, _programs = []
, _window = Nothing
--, _feather = defaultFeather
}

type Response = [SDL.Event] -> StateWit -> IO StateWit
type GameState = SDL.GLContext -> KeySet -> Response

understand :: KeySet -> Response
understand keys _events stateInfo = do
  stateInfo <- pure $ set keyset keys stateInfo
  stateInfo <- toggleOption ScancodeK isShowingKeys stateInfo
  stateInfo <- toggleOption ScancodeT isShowingTicks stateInfo
  decideState stateInfo

decideState :: StateWit -> IO StateWit
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

navigate :: StateWit -> Int -> Int -> Int
navigate stateInfo finger = mod (finger + cast (wayUpDown (stateInfo^.keyset) keyBegun))

togglePause :: StateWit -> StateWit
togglePause stateInfo = set currentState (case stateInfo^.currentState of
    Play -> Pause
    Pause -> Play
    _ -> error "bad state"
  ) stateInfo

toggleOption :: Scancode -> Lens' OptionsInfo Bool -> StateWit -> IO StateWit
toggleOption keycode lens stateInfo = pure $ if keyBegun (stateInfo^.keyset) keycode
  then set (options.lens) (not $ stateInfo^.options.lens) stateInfo
  else stateInfo