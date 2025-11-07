module Main (main, context, events) where

import Control.Lens (Lens', makeLenses, set, (^.))
import Control.Monad (unless, when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, execStateT)
import Data.Function (applyWhen)

import Foreign (Word32)
import SDL (Mode (Normal), Profile (Core), V4 (V4))
import SDL.Input.Keyboard.Codes

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL (
    initializeAll, quit, getKeyboardState, ticks
  , Event, pollEvents
  , Window, WindowConfig (..), defaultWindow, createWindow, destroyWindow
  , OpenGLConfig (..)
  , GLContext, WindowGraphicsContext (OpenGLContext), glCreateContext, glDeleteContext, glSwapWindow
  )

import Happen (unwrapHappenWindow, waxwane, unwrapHappenMouse)
import Key (KeySet, keyBegun, listen, unkeys)
import Matrix (RenderView, fromTranslation)
import Mean (ly', weep, full)
import Shade
import Time (Time, beginTime, keepTime)
import Light (Point)

import FrogState (
    News
  , Stately
  , StateName (..)
  , Settings
  , makeSettings
  , isShowingKeys
  , isRunningTests
  , isShowingTicks
  , preent
  , _update
  )
import PlayState (PlayState, makePlayState)
import PauseState (PauseState, makePauseState)
import MenuState (MenuState, makeMenuState, hand, finger)


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
--  , SDL.windowInputGrabbed = True
}

data Allwit = Allwit {
  _time :: Time
, _settings :: Settings
, _keyset :: KeySet
, _mouse :: Point
, _events :: [SDL.Event]
, _window :: SDL.Window
, _display :: RenderView
, _context :: SDL.GLContext
, _playState :: PlayState
, _pauseState :: PauseState
, _menuState :: MenuState
, _nowState :: StateName
}
makeLenses ''Allwit

news :: Allwit -> News
news allwit = (allwit^.keyset, allwit^.mouse, allwit^.display, allwit^.time)

mkAllwit
  :: SDL.Window -> RenderView -> SDL.GLContext
  -> PlayState -> PauseState -> MenuState -> StateName
  -> Allwit
mkAllwit = Allwit beginTime makeSettings unkeys (GL.Vertex2 0 0) []

main :: IO ()
main = do
  SDL.initializeAll
  _ <- SDL.getKeyboardState
  wind <- SDL.createWindow "frogwork" openGLWindow
  ctx <- SDL.glCreateContext wind

  birth wind ctx >>= execStateT live >> die wind ctx

birth :: SDL.Window -> SDL.GLContext -> IO Allwit
birth wind ctx = do
  dis <- waxwane wind
  ms <- begetMeshes

  let allwit = mkAllwit wind dis ctx
        (makePlayState ms)
        makePauseState
        makeMenuState
        MenuName

  when (allwit^.settings.isRunningTests) weep

  return allwit

begetMeshes :: IO [Mesh]
begetMeshes = do
  froggy <- createAssetMesh defaultAssetMeshProfile
    >>= flip setMeshTransform (fromTranslation [0, -2, -5])

  earth <- createSimpleMesh defaultSimpleMeshProfile

  farsee <- createAssetMesh (createAsset "tv")
    >>= flip setMeshTransform (fromTranslation [2, -2, -5])
  
  return [froggy, earth, farsee]

updateWindow :: StateT Allwit IO ()
updateWindow = get
  >>= \allwit -> when (or.unwrapHappenWindow $ allwit^.events)
    (lift (waxwane $ allwit^.window) >>= \dis -> put allwit { _display = dis })

updateMouse :: StateT Allwit IO ()
updateMouse = get
  >>= \allwit -> do
    let m = unwrapHappenMouse (allwit^.events)
    when (full m) (put allwit { _mouse = head m })

updateKeys :: StateT Allwit IO ()
updateKeys = get
  >>= \allwit -> put allwit { _keyset = listen (allwit^.events) (allwit^.keyset) }

updateTime :: IO Word32 -> StateT Allwit IO ()
updateTime now = get
  >>= \allwit -> lift now
  >>= \now' -> put allwit { _time = keepTime (allwit^.time) now' }

updateEvents :: IO [SDL.Event] -> StateT Allwit IO ()
updateEvents es = get
  >>= \allwit -> lift es
  >>= \es' -> put allwit { _events = es' }

showLeechwit :: StateT Allwit IO ()
showLeechwit = get >>= \allwit -> do
  when (allwit^.settings.isShowingKeys) (preent $ allwit^.keyset)
  when (allwit^.settings.isShowingTicks) (preent $ allwit^.time)

updateEverything :: StateT Allwit IO ()
updateEverything = do
  updateEvents SDL.pollEvents
  updateTime SDL.ticks
  updateKeys
  updateMouse
  updateWindow
  updateSettings

live :: StateT Allwit IO ()
live = do
  allwit <- get

  updateEverything
  showLeechwit

  settleState
  case allwit^.nowState of
    PlayName -> goto playState
    PauseName -> goto pauseState
    MenuName -> goto menuState

  SDL.glSwapWindow (allwit^.window)

  unless (keyBegun (allwit^.keyset) ScancodeQ) live

settleState :: StateT Allwit IO ()
settleState = do
  allwit <- get
  put allwit { _nowState =
    if keyBegun (allwit^.keyset) ScancodeP && (allwit^.nowState) == PlayName
      then PauseName
    else if keyBegun (allwit^.keyset) ScancodeP && (allwit^.nowState) == PauseName
      then PlayName
    else if keyBegun (allwit^.keyset) ScancodeReturn && (allwit^.nowState) == MenuName
      then fst ((allwit^.menuState.hand)!!(allwit^.menuState.finger))
      else allwit^.nowState
  }

goto :: Stately a => Lens' Allwit a -> StateT Allwit IO ()
goto lens = get
  >>= \allwit -> lift (execStateT (_update $ news allwit) (allwit^.lens))
  >>= \state -> put (set lens state allwit)

die :: SDL.Window -> SDL.GLContext -> IO ()
die wind ctx = do
  GL.finish
  SDL.glDeleteContext ctx
  SDL.destroyWindow wind
  SDL.quit

updateSettings :: StateT Allwit IO ()
updateSettings = do
  toggleSetting ScancodeK isShowingKeys
  toggleSetting ScancodeT isShowingTicks

toggleSetting :: Scancode -> Lens' Settings Bool -> StateT Allwit IO ()
toggleSetting keycode lens = get
  >>= \allwit -> put allwit {
    _settings = applyWhen (keyBegun (allwit^.keyset) keycode)
      (ly' (const ("setting toggled!" :: String)) $ set lens (not $ allwit^.settings.lens))
      (allwit^.settings)
  }
