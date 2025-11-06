module Main (main, context) where

import Control.Lens (Lens', makeLenses, set, (^.))
import Control.Monad (unless, when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, execStateT)
import Data.Function (applyWhen)
import Data.Maybe (fromMaybe)

import SDL (
    GLContext
  , Mode (Normal)
  , Profile (Core)
  , V4 (V4)
  , Window
  )
import SDL.Input.Keyboard.Codes

import Key
import Test
import FrogState
import MenuState
import PauseState
import Shade
import Time
import Matrix
import Mean
import Happen (unwrapHappenWindow, waxwane)

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL (
    initializeAll, quit, getKeyboardState, ticks, pollEvents
  , WindowConfig(..), defaultWindow, createWindow, destroyWindow
  , OpenGLConfig(..), glCreateContext, glDeleteContext, glSwapWindow
  , WindowGraphicsContext(OpenGLContext)
  )
import PlayState (PlayState, makePlayState, meshes)


openGLConfig :: SDL.OpenGLConfig
openGLConfig = SDL.OpenGLConfig {
    SDL.glColorPrecision = V4 8 8 8 0
  , SDL.glDepthPrecision = 24
  , SDL.glStencilPrecision = 8
  , SDL.glMultisampleSamples = 1
  , SDL.glProfile = Core Normal 2 1
}

openGLWindow :: SDL.WindowConfig
openGLWindow = SDL.defaultWindow {
  SDL.windowGraphicsContext = SDL.OpenGLContext openGLConfig,
  SDL.windowResizable = True
}

type StateTuple = (PlayState, PauseState, MenuState)

data Allwit = Allwit {
  _time :: Time,
  _settings :: Settings,
  _keyset :: KeySet,
  _window :: Window,
  _display :: RenderView,
  _context :: GLContext,
  _stateList :: StateTuple,
  _nowState :: StateName
}
makeLenses ''Allwit

getPlay :: StateTuple -> PlayState
getPlay (x, _, _) = x

getPause :: StateTuple -> PauseState
getPause (_, x, _) = x

getMenu :: StateTuple -> MenuState
getMenu (_, _, x) = x

news :: Allwit -> News
news allwit = (allwit^.keyset, allwit^.display, allwit^.time)

mkAllwit :: Window -> RenderView -> GLContext -> StateTuple -> StateName -> Allwit
mkAllwit = Allwit
  beginTime
  makeSettings
  unkeys

main :: IO ()
main = do
  SDL.initializeAll
  _ <- SDL.getKeyboardState
  wind <- SDL.createWindow "frog universe" openGLWindow
  ctx <- SDL.glCreateContext wind

  birth wind ctx >>= execStateT live >> die wind ctx

birth :: Window -> GLContext -> IO Allwit
birth wind ctx = do
  dis <- waxwane wind

  playerMesh <- createAssetMesh defaultAssetMeshProfile
    >>= flip setMeshTransform (fromTranslation [0, -2, -5])

  floorMesh <- createSimpleMesh defaultSimpleMeshProfile

  froggy <- createAssetMesh (createAsset "tv")
    >>= flip setMeshTransform (fromTranslation [2, -2, -5])

  let m = [playerMesh, floorMesh, froggy]

  let allwit = mkAllwit wind dis ctx (
        set meshes m makePlayState,
        makePauseState,
        makeMenuState
        ) Menu

  when (allwit^.settings.isRunningTests) someFand

  return allwit

updateWindow :: StateT Allwit IO ()
updateWindow = do
  allwit <- get
  dis <- lift (waxwane (allwit^.window))
  put allwit {
    _display = dis
  }

live :: StateT Allwit IO ()
live = do
  allwit <- get

  es <- SDL.pollEvents
  now <- SDL.ticks

  put allwit {
      _keyset = listen es (allwit^.keyset)
    , _time = keepTime (allwit^.time) now
  }

  when (or (unwrapHappenWindow es)) updateWindow
  when (allwit^.settings.isShowingKeys) (preent $ allwit^.keyset)
  when (allwit^.settings.isShowingTicks) (preent $ allwit^.time)

  toggleSettings
  togglePause ScancodeP

  case allwit^.nowState of
    Play -> doPlayState
    Pause -> doPauseState
    Menu -> doMenuState

  SDL.glSwapWindow (allwit^.window)

  unless (keyBegun (allwit^.keyset) ScancodeQ) live

die :: Window -> GLContext -> IO ()
die w c = do
  GL.finish
  SDL.glDeleteContext c
  SDL.destroyWindow w
  SDL.quit

doPlayState :: StateT Allwit IO ()
doPlayState = do
  allwit <- get
  newPlay <- lift $ execStateT (_update (news allwit)) $ getPlay (allwit^.stateList)
  put allwit {
    _stateList = (newPlay, getPause (allwit^.stateList), getMenu (allwit^.stateList))
  }

doPauseState :: StateT Allwit IO ()
doPauseState = do
  allwit <- get
  newPause <- lift $ execStateT (_update (news allwit)) $ getPause (allwit^.stateList)
  put allwit {
    _stateList = (getPlay (allwit^.stateList), newPause, getMenu (allwit^.stateList))
  }

doMenuState :: StateT Allwit IO ()
doMenuState = do
  allwit <- get
  newMenu <- lift $ execStateT (_update (news allwit)) $ getMenu (allwit^.stateList)
  put allwit {
    _stateList = (getPlay (allwit^.stateList), getPause (allwit^.stateList), newMenu),
    _nowState = fromMaybe (allwit^.nowState) (newMenu^.choosen)
  }

toggleSettings :: StateT Allwit IO ()
toggleSettings = do
  toggleOnlyOneSetting ScancodeK isShowingKeys
  toggleOnlyOneSetting ScancodeT isShowingTicks

toggleOnlyOneSetting :: Scancode -> Lens' Settings Bool -> StateT Allwit IO ()
toggleOnlyOneSetting keycode lens = do
  allwit <- get
  put allwit {
    _settings = applyWhen (keyBegun (allwit^.keyset) keycode)
      (ly' (const ("setting toggled!" :: String)) $ set lens (not $ allwit^.settings.lens))
      (allwit^.settings)
  }

togglePause :: Scancode -> StateT Allwit IO ()
togglePause key = do
  allwit <- get
  when (keyBegun (allwit^.keyset) key) $
    put allwit {
      _nowState = case allwit^.nowState of
        Play -> ly' (const ("paused." :: String)) Pause
        _ -> ly' (const ("not paused." :: String)) Play
    }