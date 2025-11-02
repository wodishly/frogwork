{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{- HLINT ignore "Redundant return" -}

module Main where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State
import Data.Word (Word32)

import SDL (Event)
import SDL.Vect
import SDL.Video
import SDL.Input.Keyboard.Codes
import qualified Graphics.Rendering.OpenGL as GL (get)
import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified SDL (initializeAll, quit, getKeyboardState, ticks, pollEvents)

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

data Allwit = Allwit {
  _time :: Time,
  _options :: OptionsInfo,
  _ctx :: GLContext,
  _keys :: KeySet,
  _window :: Window,
  _events :: [Event],
  _nowState :: Statewit
}
makeLenses ''Allwit

mkAllwit :: GLContext -> Window -> Allwit
mkAllwit ctx w = Allwit
  beginTime
  defaultOptions
  ctx
  unkeys
  w
  []
  makeState

main :: IO ()
main = do
  SDL.initializeAll

  window <- createWindow "frog universe" openGLWindow
  ctx <- glCreateContext window

  V2 windowWidth windowHeight <- (cast <$>) <$> GL.get (windowSize window)
  viewport $= (Position 0 0, Size windowWidth windowHeight)

  allwit <- birth ctx window

  _ <- execStateT live allwit

  die window ctx
  SDL.quit

birth :: GLContext -> Window -> IO Allwit
birth ctx w = do

  depthFunc $= Just Lequal

  playerMesh <- createAssetMesh defaultAssetMeshProfile
  playerMesh <- setMeshTransform playerMesh $ fromTranslation 0 (-2) 0

  floorMesh <- createSimpleMesh defaultSimpleMeshProfile

  froggy <- createAssetMesh $ createAsset "test"
  froggy <- setMeshTransform froggy $ fromTranslation 2 (-2) 0

  let m = [playerMesh, floorMesh, froggy]

  let allwit = mkAllwit ctx w
  put $ allwit {
    _nowState = set meshes m (allwit^.nowState)
  }

  when (allwit^.options.isRunningTests) someFand
  return allwit

live :: StateT Allwit IO ()
live = do
  allwit <- get

  events <- SDL.pollEvents
  keys <- listen (allwit^.keys) <$> SDL.getKeyboardState
  now <- SDL.ticks
  liftIO $ print "hi"

  when (allwit^.options.isShowingKeys) (liftIO $ print keys)
  when (allwit^.options.isShowingTicks) (liftIO $ print $ allwit^.time)

  understand
  ticktock now

  let thisState = allwit^.nowState
  stuff (playState $ passables allwit) thisState

  glSwapWindow (allwit^.window)

  unless (keyBegun keys ScancodeQ) live
  return ()

passables :: Allwit -> Passables
passables allwit = (allwit^.events, allwit^.keys, allwit^.window, allwit^.time)

die :: Window -> GLContext -> IO ()
die window ctx = do
  finish
  glDeleteContext ctx
  destroyWindow window
  return ()

-- stateByName :: StateName -> GameState
-- stateByName name = case name of
--   Play -> playState
--   Pause -> pauseState
--   Menu -> menuState
--   Quit -> quitState

understand :: StateT Allwit IO ()
understand = do
  toggleOption ScancodeK isShowingKeys
  toggleOption ScancodeT isShowingTicks
  -- decideState

ticktock :: Word32 -> StateT Allwit IO ()
ticktock now = do
  allwit <- get
  put $ allwit { _time = keepTime (allwit^.time) now }

toggleOption :: Scancode -> Lens' OptionsInfo Bool -> StateT Allwit IO ()
toggleOption keycode lens = do
  allwit <- get
  if keyBegun (allwit^.keys) keycode
  then put $ allwit {
    _options = set lens (not $ allwit^.options.lens) (allwit^.options)
  }
  else put allwit

togglePause :: StateT Allwit IO ()
togglePause = return ()
-- do
--   allwit <- get
--   put $ allwit { _nowState = case allwit^.nowState.currentState of
--     Play -> Pause
--     Pause -> Play
--     _ -> error "bad state" }