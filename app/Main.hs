{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{- HLINT ignore "Redundant return" -}

module Main where

import Control.Lens
import Control.Monad (unless, when)

import SDL.Vect
import SDL.Video
import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL
import qualified SDL (initializeAll, quit, getKeyboardState, ticks, pollEvents)
import SDL (eventPayload, KeyboardEventData (keyboardEventKeysym), Keysym (keysymScancode), Event, EventPayload (KeyboardEvent))

import Key
import FrogState
import Test
import MenuState
import PauseState
import PlayState
import QuitState
import Shade
import Time
import Rime
import Matrix
import Data.Maybe (mapMaybe)

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

  _ <- SDL.getKeyboardState
  stateInfo <- birth defaultState window
  live window ctx stateInfo

  die window ctx
  SDL.quit

birth :: StateWit -> Window -> IO StateWit
birth stateInfo w = do
  depthFunc $= Just Lequal

  playerMesh <- createAssetMesh defaultAssetMeshProfile
  playerMesh <- setMeshTransform playerMesh $ fromTranslation [0, -2, -5]

  floorMesh <- createSimpleMesh defaultSimpleMeshProfile

  froggy <- createAssetMesh $ createAsset "test"
  froggy <- setMeshTransform froggy $ fromTranslation [2, -2, -5]

  let m = [playerMesh, floorMesh, froggy]

  stateInfo <- pure $ set window (Just w) stateInfo
  stateInfo <- pure $ set meshes m stateInfo
  return stateInfo

unwrapKey :: Event -> Maybe Scancode
unwrapKey event = case eventPayload event of
  KeyboardEvent e -> Just $ keysymScancode $ keyboardEventKeysym e
  _ -> Nothing

live :: Window -> GLContext -> StateWit -> IO ()
live window ctx stateInfo = do
  events <- SDL.pollEvents
  now <- SDL.ticks
  print $ mapMaybe unwrapKey events
  stateInfo <- pure $ set keyset (listen (stateInfo^.keyset) (mapMaybe unwrapKey events)) stateInfo

  let keys = stateInfo^.keyset


  when (stateInfo^.options.isShowingKeys) (print keys)
  when (stateInfo^.options.isShowingTicks) (print $ stateInfo^.time)

  stateInfo <- understand keys stateInfo
  stateInfo <- pure $ set time (keepTime (stateInfo^.time) now) stateInfo
  stateInfo <- (stateByName $ stateInfo^.currentState) ctx keys stateInfo

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
