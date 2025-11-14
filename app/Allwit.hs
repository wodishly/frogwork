module Allwit (
  Allwit (..)
, Overwindow
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
, waxwane
) where

import Control.Lens (Lens', makeLenses, (.~), (^.))
import Control.Monad (unless, void, when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, execStateT)

import SDL (LocationMode (AbsoluteLocation, RelativeLocation))
import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (
    BlendingFactor (OneMinusSrcAlpha, SrcAlpha)
  , Capability (Enabled)
  , ComparisonFunction (Lequal)
  , HasSetter (($=))
  , Position (Position)
  , Size (Size)
  , Vertex2 (Vertex2)
  , Vertex3 (Vertex3)
  )
import qualified Graphics.Rendering.OpenGL as GL (get, blend, blendFunc, depthFunc, viewport)

import qualified SDL (
    Event
  , V2 (V2)
  , Window
  , glSwapWindow
  , pollEvents
  , setMouseLocationMode
  , ticks
  , windowGrab
  , windowSize
  )

import State (
    News
  , Stately (name)
  , StateName (..)
  , Settings
  , makeSettings
  , isShowingKeys
  , isShowingTicks
  , isRunningTests
  , loop
  , preent, toggle
  )
import TitleState (TitleState (finger, hand), makeTitleState)
import WillState (WillState (choosen), makeWillState, unchoose)
import PauseState (PauseState, makePauseState)
import PlayState (PlayState, makePlayState)
import EndState (EndState, makeEndState)

import FastenShade (
    ShaderProfile (..)
  , SimpleMeshProfile (..)

  , defaultSimpleMeshProfile
  , iBuffer
  , quadUvBuffer
  )

import Matrix (RenderView (..), fromAffine, fromTranslation)
import Mean (twimap, full, weep, ssss, ly)
import Time (Time, beginTime, keepTime)

import Happen (Mousewit, Overwindow, unwrapHappenPointer, unwrapHappenWheel, unwrapHappenWindow)
import Key (Keyset, anyKeysBegun, keyBegun, listen, unkeys)

import MothSpell (mothify)
import Skeleton (Animation (..), makeAnimation, play, evermore)
import Shade (Mesh (meshAnimation), makeAsset, makeAssetMesh, makeSimpleMesh, setMeshTransform)
import Spell (summon, unwrappingly)
import Stavemake (Staveware, makeFeather)


data Allwit = Allwit {
  overwindow :: Overwindow
, settings :: Settings
, events :: [SDL.Event]
, staveware :: Staveware

, keyset :: Keyset
, mouse :: Mousewit
, display :: RenderView
, time :: Time

, nowState :: StateName
, _titleState :: TitleState
, _willState :: WillState
, _playState :: PlayState
, _pauseState :: PauseState
, _endState :: EndState
}
makeLenses ''Allwit

window :: Allwit -> SDL.Window
window = fst . overwindow

makeAllwit :: Overwindow -> Staveware -> RenderView -> [Mesh] -> Allwit
makeAllwit overwind ware dis meshes = let sets = makeSettings in
  Allwit
    overwind
    sets
    []
    ware
    unkeys
    (Vertex2 0 0, Vertex2 0 0)
    dis
    beginTime
    TitleName
    (makeTitleState ware)
    (makeWillState ware sets)
    (makePlayState ware meshes)
    (makePauseState ware)
    (makeEndState ware)

begetMeshes :: IO (Staveware, [Mesh])
begetMeshes = do
  cocoon <- summon "assets/bunny.moth"
  let mothFile = unwrappingly mothify cocoon

  bun <- makeAssetMesh $ makeAsset "bunny"
  bunAnimation' <- makeAnimation mothFile
  bunAnimation <- (play . evermore) bunAnimation' 5
  froggy <- setMeshTransform (fromAffine [1.0, 1.0, 1.0] [0, 0, 0]) $
    bun { meshAnimation = Just bunAnimation }

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
news allwit = (keyset allwit, mouse allwit, display allwit, time allwit)

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
  let p = unwrapHappenPointer (events allwit)
      w = unwrapHappenWheel (events allwit)
  put allwit { mouse = twimap (\x -> if full x then head x else Vertex2 0 0) (p, w) }

updateWindow :: StateT Allwit IO ()
updateWindow = do
  allwit <- get
  when (or . unwrapHappenWindow $ events allwit) $ do
    dis <- lift (waxwane $ window allwit)
    put allwit { display = dis }

updateSettings :: StateT Allwit IO ()
updateSettings = do
  allwit <- get
  when (keyBegun (keyset allwit) ScancodeK) $ updateSetting isShowingKeys
  when (keyBegun (keyset allwit) ScancodeT) $ updateSetting isShowingTicks
  case choosen (allwit^.willState) of
    Just (Right ch) -> do
      let newset = ch (settings allwit)
      state <- lift (execStateT (unchoose $ Just newset) $ allwit^.willState)
      put allwit {
          settings = newset
        , _willState = state
      }
    Just (Left ch) -> do
      put allwit { nowState = ch }
      state <- lift (execStateT (unchoose Nothing) $ allwit^.willState)
      put allwit {
          nowState = ch
        , _willState = state
      }
    _ -> return ()

updateAll :: StateT Allwit IO ()
updateAll = do
  updateEvents
  updateTime
  updateKeys
  updateMouse
  updateWindow
  updateSettings

updateSetting :: Lens' Settings Bool -> StateT Allwit IO ()
updateSetting lens = do
  allwit <- get
  put allwit { settings = toggle lens (settings allwit) }

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
    if keyBegun (keyset allwit) ScancodeR
      then TitleName
    else if nowState allwit == PlayName && keyBegun (keyset allwit) ScancodeP
      then PauseName
    else if nowState allwit == PauseName && keyBegun (keyset allwit) ScancodeP
      then PlayName
    else if nowState allwit == TitleName && keyBegun (keyset allwit) ScancodeReturn
      then ssss ((!!) . hand) finger (_titleState allwit)

    else nowState allwit
  }
  case nowState allwit of
    TitleName -> goto titleState
    WillName -> goto willState
    PlayName -> goto playState
    PauseName -> goto pauseState
    EndName -> goto endState

setWindowGrab :: Bool -> StateT Allwit IO ()
setWindowGrab setting = do
  allwit <- get
  SDL.windowGrab (window allwit) $= setting
  void $ SDL.setMouseLocationMode $ if setting then RelativeLocation else AbsoluteLocation

goto :: Stately a => Lens' Allwit a -> StateT Allwit IO ()
goto lens = do
  allwit <- get
  setWindowGrab (PlayName == name (allwit^.lens))
  state <- lift (execStateT (loop $ news allwit) $ allwit^.lens)
  put $ (lens.~state) allwit

blit :: StateT Allwit IO ()
blit = get >>= SDL.glSwapWindow . window

again :: StateT Allwit IO () -> StateT Allwit IO ()
again f = do
  allwit <- get
  unless (EndName == nowState allwit || anyKeysBegun (keyset allwit) [ScancodeQ, ScancodeEscape]) f

waxwane :: SDL.Window -> IO RenderView
waxwane wind = do
  SDL.V2 width height <- (fromIntegral <$>) <$> GL.get (SDL.windowSize wind)
  GL.viewport $= (Position 0 0, Size width height)
  GL.depthFunc $= Just Lequal
  GL.blend $= Enabled
  GL.blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  return RenderView {
      aspect = fromIntegral width / fromIntegral height
    , size = (fromIntegral width, fromIntegral height)
    , fov = pi / 4.0
    , near = 0.1
    , far = 100.0
  }
