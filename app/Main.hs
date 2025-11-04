{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{- HLINT ignore "Redundant return" -}

module Main where

import Data.Map (Map, fromList, (!), adjust)
import Data.Function (applyWhen)
import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.State

import SDL (Event)
import SDL.Vect
import SDL.Video
import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified Graphics.Rendering.OpenGL as GL (get)
import qualified SDL (initializeAll, quit, getKeyboardState, ticks, pollEvents)

import Key
import Test
import FrogState
import MenuState
import PauseState
import PlayState
import Shade
import Time
import Rime
import Matrix
import Mean

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

type StateTuple = (PlayState, PauseState, MenuState)

data Allwit = Allwit {
  _time :: Time,
  _settings :: Settings,
  _events :: [Event],
  _keyset :: KeySet,
  _ctx :: GLContext,
  _window :: Window,
  _stateList :: StateTuple,
  _nowState :: StateName
}
makeLenses ''Allwit

news :: Allwit -> News
news allwit = (allwit^.events, allwit^.keyset, allwit^.window, allwit^.time)

mkAllwit :: GLContext -> Window -> StateTuple -> StateName -> Allwit
mkAllwit = Allwit
  beginTime
  makeSettings
  []
  unkeys

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

wake :: Stately a => StateT a IO ()
wake = return ()

birth :: GLContext -> Window -> IO Allwit
birth ctx w = do

  depthFunc $= Just Lequal

  playerMesh <- createAssetMesh defaultAssetMeshProfile
  playerMesh <- setMeshTransform playerMesh $ fromTranslation 0 (-2) 0

  floorMesh <- createSimpleMesh defaultSimpleMeshProfile

  froggy <- createAssetMesh $ createAsset "test"
  froggy <- setMeshTransform froggy $ fromTranslation 2 (-2) 0

  let m = [playerMesh, floorMesh, froggy]

  play <- execStateT wake makePlayState
  play <- pure $ set meshes m play

  pause <- execStateT wake makePauseState

  menu <- execStateT wake makeMenuState

  let allwit = mkAllwit ctx w (play, pause, menu) Menu

  when (allwit^.settings.isRunningTests) someFand

  return allwit

live :: StateT Allwit IO ()
live = do
  allwit <- get

  events <- SDL.pollEvents
  keys <- listen unkeys <$> SDL.getKeyboardState
  now <- SDL.ticks

  put $ allwit {
    _events = events,
    _keyset = keys,
    _time = keepTime (allwit^.time) now
  }

  when (allwit^.settings.isShowingKeys) (lift $ print $ allwit^.keyset)
  when (allwit^.settings.isShowingTicks) (lift $ print $ allwit^.time)

  toggleSettings
  togglePause ScancodeP

  when (allwit^.nowState == Menu) (do
    let menu = (\(_,_,x) -> x) (allwit^.stateList) in do
      case MenuState._chosen menu of
        Just _ -> do
          put $ allwit { _nowState = Play }
          lift $ print "yayyaayaya"
        Nothing -> lift weep
      _ <- lift $ execStateT (_update (news allwit)) menu
      return ()
    )

  -- _ <- lift $ case (allwit^.stateList)!(allwit^.nowState) of
  --   Pa x f -> do
  --     newState <- execStateT (f (news allwit)) x
  --     return $ (put :: Allwit -> StateT Allwit IO ()) $ allwit {
  --       _stateList = adjust (const $ Pa newState f) (allwit^.nowState) (allwit^.stateList)
  --     }
  --   Pl x f -> do
  --     newState <- execStateT (f (news allwit)) x
  --     return $ put $ allwit {
  --       _stateList = adjust (const $ Pl newState f) (allwit^.nowState) (allwit^.stateList)
  --     }
  --   Me x f -> do
  --     newState <- execStateT (f (news allwit)) x
  --     return $ put $ allwit {
  --       _stateList = adjust (const $ Me newState f) (allwit^.nowState) (allwit^.stateList)
  --     }

  glSwapWindow (allwit^.window)

  unless (keyBegun (allwit^.keyset) ScancodeQ) live
  return ()

die :: Window -> GLContext -> IO ()
die window ctx = do
  finish
  glDeleteContext ctx
  destroyWindow window
  return ()

toggleSettings :: StateT Allwit IO ()
toggleSettings = do
  toggleOnlyOneSetting ScancodeK isShowingKeys
  toggleOnlyOneSetting ScancodeT isShowingTicks

toggleOnlyOneSetting :: Scancode -> Lens' Settings Bool -> StateT Allwit IO ()
toggleOnlyOneSetting keycode lens = do
  allwit <- get
  put $ allwit {
    _settings = applyWhen (keyBegun (allwit^.keyset) keycode)
      (ly' (const ("setting toggled!" :: String)) $ set lens (not $ allwit^.settings.lens))
      (allwit^.settings)
  }

togglePause :: Scancode -> StateT Allwit IO ()
togglePause key = do
  allwit <- get
  when (keyBegun (allwit^.keyset) key) $
    put $ allwit {
      _nowState = case allwit^.nowState of
        Play -> ly' (const ("paused." :: String)) Pause
        _ -> ly' (const ("not paused." :: String)) Play
    }