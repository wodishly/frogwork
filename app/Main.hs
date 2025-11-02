{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Lens
import Control.Monad (unless, when)

import SDL.Vect
import SDL.Video
import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL
import qualified SDL (initializeAll, quit, getKeyboardState, ticks, pollEvents)

import Key
import State
import Test
import MenuState
import PauseState
import PlayState
import QuitState
import Shade
import Time
import Rime

openGLConfig :: OpenGLConfig
openGLConfig = OpenGLConfig {
    glColorPrecision = V4 8 8 8 0
  , glDepthPrecision = 24
  , glStencilPrecision = 8
  , glMultisampleSamples = 1
  , glProfile = Core Normal 2 1
}

openGLWindow :: WindowConfig
openGLWindow = defaultWindow {
  windowGraphicsContext = OpenGLContext openGLConfig,
  windowResizable = True
}

main :: IO ()
main = do
  when (defaultState^.options.isRunningTests) someFand

  SDL.initializeAll

  window <- createWindow "frog universe" openGLWindow
  ctx <- glCreateContext window

  V2 windowWidth windowHeight <- (cast <$>) <$> get (windowSize window)
  viewport $= (Position 0 0, Size windowWidth windowHeight)

  stateInfo <- birth defaultState window
  live window ctx stateInfo

  die window ctx
  SDL.quit

birth :: StateInfo -> Window -> IO StateInfo
birth stateInfo w = do
  depthFunc $= Just Lequal
  
  playerMesh <- createAssetMesh defaultAssetMeshProfile
  playerMesh <- setMeshTransform playerMesh $ fromTranslation 0 (-2) 0

  floorMesh <- createSimpleMesh defaultSimpleMeshProfile

  froggy <- createAssetMesh $ createAsset "test"
  froggy <- setMeshTransform froggy $ fromTranslation 2 (-2) 0

  let m = [playerMesh, floorMesh, froggy]

  let stateWithWindow = set window (Just w) stateInfo
  return $ set meshes m stateWithWindow

live :: Window -> GLContext -> StateInfo -> IO ()
live window ctx stateInfo = do
  events <- SDL.pollEvents
  keys <- listen (stateInfo^.keyset) <$> SDL.getKeyboardState
  now <- SDL.ticks

  when (stateInfo^.options.isShowingKeys) (print keys)
  when (stateInfo^.options.isShowingTicks) (print $ stateInfo^.time)

  stateInfo <- understand keys events stateInfo
  stateInfo <- pure $ set time (keepTime (stateInfo^.time) now) stateInfo
  stateInfo <- (stateByName $ stateInfo^.currentState) ctx keys events stateInfo

  glSwapWindow window

  unless (keyBegun keys ScancodeQ) (live window ctx stateInfo)

die :: Window -> GLContext -> IO ()
die window ctx = do
  finish
  glDeleteContext ctx
  destroyWindow window
  return ()

stateByName :: StateName -> GameState
stateByName name = case name of
  Play -> playState
  Pause -> pauseState
  Menu -> menuState
  Quit -> quitState