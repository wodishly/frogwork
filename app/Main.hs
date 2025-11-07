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

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL (
    initializeAll, quit, getKeyboardState, ticks, pollEvents
  , WindowConfig (..), defaultWindow, createWindow, destroyWindow
  , OpenGLConfig (..), glCreateContext, glDeleteContext, glSwapWindow
  , WindowGraphicsContext (OpenGLContext)
  )

import FrogState
import PlayState
import PauseState
import MenuState

import Happen (unwrapHappenWindow, waxwane)
import Key (KeySet, keyBegun, listen, unkeys)
import Matrix (RenderView, fromTranslation)
import Mean (ly', weep)
import Shade
import Time (Time, beginTime, keepTime)


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
    SDL.windowGraphicsContext = SDL.OpenGLContext openGLConfig
  , SDL.windowResizable = True
}

data Allwit = Allwit {
    _time :: Time
  , _settings :: Settings
  , _keyset :: KeySet
  , _window :: Window
  , _display :: RenderView
  , _context :: GLContext
  , _playState :: PlayState
  , _pauseState :: PauseState
  , _menuState :: MenuState
  , _nowState :: StateName
}
makeLenses ''Allwit

news :: Allwit -> News
news allwit = (allwit^.keyset, allwit^.display, allwit^.time)

mkAllwit
  :: Window -> RenderView -> GLContext
  -> PlayState -> PauseState -> MenuState -> StateName
  -> Allwit
mkAllwit = Allwit beginTime makeSettings unkeys

main :: IO ()
main = do
  SDL.initializeAll
  _ <- SDL.getKeyboardState
  wind <- SDL.createWindow "frogwork" openGLWindow
  ctx <- SDL.glCreateContext wind

  birth wind ctx >>= execStateT live >> die wind ctx

birth :: Window -> GLContext -> IO Allwit
birth wind ctx = do
  dis <- waxwane wind

  froggy <- createAssetMesh defaultAssetMeshProfile
    >>= flip setMeshTransform (fromTranslation [0, -2, -5])

  earth <- createSimpleMesh defaultSimpleMeshProfile

  farsee <- createAssetMesh (createAsset "tv")
    >>= flip setMeshTransform (fromTranslation [2, -2, -5])

  let m = [froggy, earth, farsee]

  let allwit = mkAllwit wind dis ctx
        (set meshes m makePlayState)
        makePauseState
        makeMenuState
        MenuName

  when (allwit^.settings.isRunningTests) weep

  return allwit

updateWindow :: StateT Allwit IO ()
updateWindow = do
  allwit <- get
  dis <- lift (waxwane $ allwit^.window)
  put allwit { _display = dis }

live :: StateT Allwit IO ()
live = do
  allwit <- get

  es <- SDL.pollEvents
  now <- SDL.ticks

  put allwit {
      _keyset = listen es (allwit^.keyset)
    , _time = keepTime (allwit^.time) now
  }

  when (or $ unwrapHappenWindow es) updateWindow
  when (allwit^.settings.isShowingKeys) (preent $ allwit^.keyset)
  when (allwit^.settings.isShowingTicks) (preent $ allwit^.time)

  toggleSettings
  togglePause ScancodeP

  case allwit^.nowState of
    PlayName -> goto playState
    PauseName -> goto pauseState
    MenuName -> menu

  SDL.glSwapWindow (allwit^.window)

  unless (keyBegun (allwit^.keyset) ScancodeQ) live

goto :: Stately a => Lens' Allwit a -> StateT Allwit IO ()
goto lens = get >>= \allwit -> lift (execStateT (_update $ news allwit) (allwit^.lens))
  >>= \state -> put (set lens state allwit)

menu :: StateT Allwit IO ()
menu = get >>= \allwit -> lift (execStateT (_update $ news allwit) (allwit^.menuState))
  >>= \state -> put allwit {
    _menuState = state
  , _nowState = fromMaybe (allwit^.nowState) (state^.choosen)
  }

die :: Window -> GLContext -> IO ()
die w c = do
  GL.finish
  SDL.glDeleteContext c
  SDL.destroyWindow w
  SDL.quit

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
        PlayName -> ly' (const ("paused." :: String)) PauseName
        _ -> ly' (const ("not paused." :: String)) PlayName
    }
