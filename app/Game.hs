module Game (
  Allwit (..)
, makeAllwit
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
import Control.Monad (unless, when, void)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, execStateT)
import Data.Function (applyWhen)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2 (Vertex2), Vertex3 (Vertex3))

import qualified SDL (
    Event
  , GLContext
  , LocationMode (AbsoluteLocation)
  , Window
  , glSwapWindow
  , pollEvents
  , setMouseLocationMode
  , ticks
  , windowGrab
  )

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
import MenuState (MenuState (hand, finger))
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
import Matrix (RenderView, fromTranslation)
import Mean (full, weep)
import Rime (Point)
import SDL (LocationMode (RelativeLocation), ($=))
import Shade (Mesh, makeAsset, makeAssetMesh, makeSimpleMesh, setMeshTransform)
import Stave (Staveware, makeFeather)
import Time (Time, beginTime, keepTime)


data Allwit = Allwit {
  time :: Time
, settings :: Settings
, keyset :: KeySet
, mouse :: Point
, wheel :: Point
, events :: [SDL.Event]
, nowState :: StateName

, staveware :: Staveware
, window :: SDL.Window
, display :: RenderView
, context :: SDL.GLContext

, _playState :: PlayState
, _pauseState :: PauseState
, _menuState :: MenuState
}
makeLenses ''Allwit

makeAllwit :: Staveware -> SDL.Window -> RenderView -> SDL.GLContext
  -> PlayState -> PauseState -> MenuState -> Allwit
makeAllwit = Allwit
  beginTime
  makeSettings
  unkeys
  (Vertex2 0 0)
  (Vertex2 0 0)
  []
  MenuName

begetMeshes :: IO (Staveware, [Mesh])
begetMeshes = do
  froggy <- makeAssetMesh defaultAssetMeshProfile
    >>= setMeshTransform (fromTranslation [0, 0, 0])

  earth <- makeSimpleMesh defaultSimpleMeshProfile

  farsee <- makeAssetMesh (makeAsset "tv")
    >>= setMeshTransform (fromTranslation [-2, 1, 2])

  hack <- makeSimpleMesh $ SimpleMeshProfile {
      vbuffer = [Vertex3 1 1 0, Vertex3 1 -1 0, Vertex3 -1 -1 0, Vertex3 -1 1 0]
    , ibuffer = iBuffer
    , uvbuffer = Just quadUvBuffer
    , meshShaderProfile = ShaderProfile
        ("vertex_stave", "fragment_stave")
        ["u_texture", "u_time", "u_orthographic_matrix", "u_blee"]
    , texObject = Nothing
  }

  x <- makeFeather "noto-sans"
  return ((x, hack), [froggy, earth, farsee, hack])

news :: Allwit -> News
news allwit = (keyset allwit, mouse allwit, wheel allwit, display allwit, time allwit)

updateEvents :: StateT Allwit IO ()
updateEvents = do
  allwit <- get
  es <- lift SDL.pollEvents
  put allwit { events = es }

updateTime :: StateT Allwit IO ()
updateTime = do
  allwit <- get
  now <- lift SDL.ticks
  put allwit { time = keepTime (time allwit) now }

updateKeys :: StateT Allwit IO ()
updateKeys = do
  allwit <- get
  put allwit { keyset = listen (events allwit) (keyset allwit) }

updateMouse :: StateT Allwit IO ()
updateMouse = do
  allwit <- get
  -- todo sum instead of head
  let m = unwrapHappenMouse (events allwit)
  if full m
    then put allwit { mouse = head m }
    else put allwit { mouse = Vertex2 0 0}

updateWheel :: StateT Allwit IO ()
updateWheel = do
  allwit <- get
  let w = unwrapHappenWheel (events allwit)
  if full w
    then put allwit { wheel = head w }
    else put allwit { wheel = Vertex2 0 0}

updateWindow :: StateT Allwit IO ()
updateWindow = do
  allwit <- get
  when (or.unwrapHappenWindow $ events allwit) $ do
    dis <- lift (waxwane $ window allwit)
    put allwit { display = dis }

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
  put allwit { settings =
    applyWhen (keyBegun (keyset allwit) keycode)
      (lens.~not (settings allwit^.lens))
      (settings allwit)
  }

showLeechwit :: StateT Allwit IO ()
showLeechwit = do
  allwit <- get
  when (settings allwit^.isShowingKeys) (preent $ keyset allwit)
  when (settings allwit^.isShowingTicks) (preent $ time allwit)

fand :: Allwit -> IO ()
fand = ($ weep) . when . (^.isRunningTests) . settings

settleState :: StateT Allwit IO ()
settleState = do
  allwit <- get
  put allwit { nowState =
    if keyBegun (keyset allwit) ScancodeP && nowState allwit == PlayName
      then PauseName
    else if keyBegun (keyset allwit) ScancodeP && nowState allwit == PauseName
      then PlayName
    else if keyBegun (keyset allwit) ScancodeReturn && nowState allwit == MenuName
      then fst (hand (_menuState allwit)!!finger (_menuState allwit))
      else nowState allwit
  }
  case nowState allwit of
    PlayName -> do
      setWindowGrab True
      goto playState
    PauseName -> do
      setWindowGrab False
      goto pauseState
    MenuName -> do
      setWindowGrab False
      goto menuState

setWindowGrab :: Bool -> StateT Allwit IO ()
setWindowGrab setting =
  get >>= ($= setting) . SDL.windowGrab . window
  >> void (SDL.setMouseLocationMode $ if setting then SDL.RelativeLocation else SDL.AbsoluteLocation)

goto :: Stately a => Lens' Allwit a -> StateT Allwit IO ()
goto lens = do
  allwit <- get
  state <- lift (execStateT (loop $ news allwit) $ allwit^.lens)
  put ((lens.~state) allwit)

blit :: StateT Allwit IO ()
blit = get >>= SDL.glSwapWindow . window

again :: StateT Allwit IO () -> StateT Allwit IO ()
again f = do
  allwit <- get
  unless (anyKeysBegun (keyset allwit) [ScancodeQ, ScancodeEscape]) f
