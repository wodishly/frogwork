module Main where

import Control.Lens (Lens', makeLenses, (^.), set)
import Control.Monad (unless, when)
import Control.Monad.State (StateT, execStateT, MonadTrans (lift), MonadState (get, put))
import Data.Maybe (fromMaybe)
import Data.Function (applyWhen)

import SDL (
    V2(V2), V4(V4)
  , Event
  , Window
  , GLContext, Profile(Core), Mode(Normal)
  )
import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (
  Size(Size),
  Position(Position),
  ComparisonFunction(Lequal),
  HasSetter(($=))
  )

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

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL (
    initializeAll, quit, getKeyboardState, ticks, pollEvents
  , WindowConfig(..), defaultWindow, createWindow, destroyWindow, windowSize
  , OpenGLConfig(..), glCreateContext, glDeleteContext, glSwapWindow
  , WindowGraphicsContext(OpenGLContext)
  )


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
  _events :: [Event],
  _keyset :: KeySet,
  _context :: GLContext,
  _window :: Window,
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
news allwit = (allwit^.keyset, allwit^.window, allwit^.time)

mkAllwit :: GLContext -> Window -> StateTuple -> StateName -> Allwit
mkAllwit = Allwit
  beginTime
  makeSettings
  []
  unkeys

main :: IO ()
main = do
  SDL.initializeAll
  _ <- SDL.getKeyboardState

  w <- SDL.createWindow "frog universe" openGLWindow
  c <- SDL.glCreateContext w

  V2 windowWidth windowHeight <- (cast <$>) <$> GL.get (SDL.windowSize w)
  GL.viewport $= (Position 0 0, Size windowWidth windowHeight)

  allwit <- birth w c

  _ <- execStateT live allwit

  die w c
  SDL.quit

wake :: Stately a => StateT a IO ()
wake = return ()

birth :: Window -> GLContext -> IO Allwit
birth w c = do

  GL.depthFunc $= Just Lequal

  playerMesh <- createAssetMesh defaultAssetMeshProfile
    >>= flip setMeshTransform (fromTranslation [0, -2, 0])

  floorMesh <- createSimpleMesh defaultSimpleMeshProfile

  froggy <- createAssetMesh (createAsset "test")
    >>= flip setMeshTransform (fromTranslation [2, -2, 0])

  let m = [playerMesh, floorMesh, froggy]

  let allwit = mkAllwit c w (
        set meshes m makePlayState,
        makePauseState,
        makeMenuState
        ) Menu

  when (allwit^.settings.isRunningTests) someFand

  return allwit

live :: StateT Allwit IO ()
live = do
  allwit <- get

  es <- SDL.pollEvents
  now <- SDL.ticks

  put $ allwit {
    _events = es,
    _keyset = listen es (allwit^.keyset),
    _time = keepTime (allwit^.time) now
  }

  when (allwit^.settings.isShowingKeys) (lift $ print $ allwit^.keyset)
  when (allwit^.settings.isShowingTicks) (lift $ print $ allwit^.time)

  toggleSettings
  togglePause ScancodeP

  case allwit^.nowState of
    Play -> doPlayState
    Pause -> doPauseState
    Menu -> doMenuState

  SDL.glSwapWindow (allwit^.window)

  unless (keyBegun (allwit^.keyset) ScancodeQ) live

doPlayState :: StateT Allwit IO ()
doPlayState = do
  allwit <- get
  newPlay <- lift $ execStateT (_update (news allwit)) $ getPlay (allwit^.stateList)
  put $ allwit {
    _stateList = (newPlay, getPause (allwit^.stateList), getMenu (allwit^.stateList))
  }

doPauseState :: StateT Allwit IO ()
doPauseState = do
  allwit <- get
  newPause <- lift $ execStateT (_update (news allwit)) $ getPause (allwit^.stateList)
  put $ allwit {
    _stateList = (getPlay (allwit^.stateList), newPause, getMenu (allwit^.stateList))
  }

doMenuState :: StateT Allwit IO ()
doMenuState = do
  allwit <- get
  newMenu <- lift $ execStateT (_update (news allwit)) $ getMenu (allwit^.stateList)
  put $ allwit {
    _stateList = (getPlay (allwit^.stateList), getPause (allwit^.stateList), newMenu),
    _nowState = fromMaybe (allwit^.nowState) (newMenu^.choosen)
  }

die :: Window -> GLContext -> IO ()
die w c = do
  GL.finish
  SDL.glDeleteContext c
  SDL.destroyWindow w
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