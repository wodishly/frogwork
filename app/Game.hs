module Game (
  Allwit (..)
, makeAllwit
, staveware -- unused
, context -- unused
, keyset
, settings
, window

, pauseState
, playState
, menuState
, nowState

, begetMeshes

, fand
, goto
, again
, news
, blit
, settleState
, showLeechwit
, updateAll
) where

import Control.Lens (Lens', makeLenses, (.~), (^.))
import Control.Monad (unless, when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, execStateT)
import Data.Function (applyWhen)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2 (Vertex2), Vertex3 (Vertex3))

import qualified SDL (Event, GLContext, Window, glSwapWindow, pollEvents, ticks)

import State (
    News
  , Stately
  , StateName (..)
  , Settings
  , makeSettings
  , isShowingKeys
  , isShowingTicks
  , isRunningTests
  , loop
  , preent
  )
import MenuState (MenuState, finger, hand)
import PauseState (PauseState)
import PlayState (PlayState)

import FastenShade (
    ShaderProfile (..)
  , SimpleMeshProfile (..)
  , defaultAssetMeshProfile
  , defaultSimpleMeshProfile
  
  , iBuffer
  , quadUvBuffer
  )

import Happen (unwrapHappenMouse, unwrapHappenWheel, unwrapHappenWindow, waxwane)
import Key (KeySet, anyKeysBegun, keyBegun, listen, unkeys)
import Matrix (Point, RenderView, fromTranslation)
import Mean (full, weep)
import Shade (Mesh, makeAsset, makeAssetMesh, makeSimpleMesh, setMeshTransform)
import Stave (Staveware, makeFeather, Staveware)
import Time (Time, beginTime, keepTime)


data Allwit = Allwit {
  _time :: Time
, _settings :: Settings
, _keyset :: KeySet
, _mouse :: Point
, _wheel :: Point
, _events :: [SDL.Event]
, _staveware :: Staveware
, _window :: SDL.Window
, _display :: RenderView
, _context :: SDL.GLContext

, _playState :: PlayState
, _pauseState :: PauseState
, _menuState :: MenuState

, _nowState :: StateName
}
makeLenses ''Allwit

makeAllwit :: Staveware -> SDL.Window -> RenderView -> SDL.GLContext
  -> PlayState -> PauseState -> MenuState -> StateName -> Allwit
makeAllwit = Allwit
  beginTime
  makeSettings
  unkeys
  (Vertex2 0 0)
  (Vertex2 0 0)
  []

begetMeshes :: IO (Staveware, [Mesh])
begetMeshes = do
  froggy <- makeAssetMesh defaultAssetMeshProfile
    >>= setMeshTransform (fromTranslation [0, 0, 0])

  earth <- makeSimpleMesh defaultSimpleMeshProfile

  farsee <- makeAssetMesh (makeAsset "tv")
    >>= setMeshTransform (fromTranslation [-2, 1, 2])

  x <- makeFeather "noto-sans"
  hack <- makeSimpleMesh $ SimpleMeshProfile {
      vbuffer = [Vertex3 1 1 0, Vertex3 1 -1 0, Vertex3 -1 -1 0, Vertex3 -1 1 0]
    , ibuffer = iBuffer
    , uvbuffer = Just quadUvBuffer
    , meshShaderProfile = ShaderProfile ("vertex_stave", "fragment_stave") ["u_texture", "u_projection_matrix"]
    , texObject = Nothing
  }

  return ((x, hack), [froggy, earth, farsee])

news :: Allwit -> News
news allwit = (allwit^.keyset, allwit^.mouse, allwit^.wheel, allwit^.display, allwit^.time)

updateEvents :: StateT Allwit IO ()
updateEvents = do
  allwit <- get
  es <- lift SDL.pollEvents
  put allwit { _events = es }

updateTime :: StateT Allwit IO ()
updateTime = do
  allwit <- get
  now <- lift SDL.ticks
  put allwit { _time = keepTime (allwit^.time) now }

updateKeys :: StateT Allwit IO ()
updateKeys = do
  allwit <- get
  put allwit { _keyset = listen (allwit^.events) (allwit^.keyset) }

updateMouse :: StateT Allwit IO ()
updateMouse = do
  allwit <- get
  -- todo sum instead of head
  let m = unwrapHappenMouse (allwit^.events)
  if full m
    then put allwit { _mouse = head m }
    else put allwit { _mouse = Vertex2 0 0}

updateWheel :: StateT Allwit IO ()
updateWheel = do
  allwit <- get
  let w = unwrapHappenWheel (allwit^.events)
  if full w
    then put allwit { _wheel = head w }
    else put allwit { _wheel = Vertex2 0 0}

updateWindow :: StateT Allwit IO ()
updateWindow = do
  allwit <- get
  when (or.unwrapHappenWindow $ allwit^.events) $ do
    dis <- lift (waxwane $ allwit^.window)
    put allwit { _display = dis }

updateSettings :: StateT Allwit IO ()
updateSettings = do
  toggleSetting ScancodeK isShowingKeys
  toggleSetting ScancodeT isShowingTicks

updateAll :: StateT Allwit IO ()
updateAll = do
  updateEvents
  updateTime
  updateKeys
  updateMouse
  updateWheel
  updateWindow
  updateSettings

toggleSetting :: Scancode -> Lens' Settings Bool -> StateT Allwit IO ()
toggleSetting keycode lens = do
  allwit <- get
  put allwit { _settings =
    applyWhen (keyBegun (allwit^.keyset) keycode)
      (lens.~not (allwit^.settings.lens))
      (allwit^.settings)
  }

showLeechwit :: StateT Allwit IO ()
showLeechwit = do
  allwit <- get
  when (allwit^.settings.isShowingKeys) (preent $ allwit^.keyset)
  when (allwit^.settings.isShowingTicks) (preent $ allwit^.time)

fand :: Allwit -> IO ()
fand allwit = when (allwit^.settings.isRunningTests) weep

settleState :: StateT Allwit IO ()
settleState = do
  allwit <- get
  put allwit { _nowState =
    if keyBegun (allwit^.keyset) ScancodeP && allwit^.nowState == PlayName
      then PauseName
    else if keyBegun (allwit^.keyset) ScancodeP && allwit^.nowState == PauseName
      then PlayName
    else if keyBegun (allwit^.keyset) ScancodeReturn && allwit^.nowState == MenuName
      then fst ((allwit^.menuState.hand)!!(allwit^.menuState.finger))
      else allwit^.nowState
  }
  case allwit^.nowState of
    PlayName -> goto playState
    PauseName -> goto pauseState
    MenuName -> goto menuState

goto :: Stately a => Lens' Allwit a -> StateT Allwit IO ()
goto lens = do
  allwit <- get
  state <- lift (execStateT (loop $ news allwit) $ allwit^.lens)
  put ((lens.~state) allwit)

blit :: StateT Allwit IO ()
blit = get >>= SDL.glSwapWindow . (^.window)

again :: StateT Allwit IO () -> StateT Allwit IO ()
again f = get >>= \allwit -> unless (anyKeysBegun (allwit^.keyset) [ScancodeQ, ScancodeEscape]) f
