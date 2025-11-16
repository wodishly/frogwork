module Allwit (
  Allwit (..)
, Setting (..)
, Settings
, makeAllwit
, begetMeshes
, fand
, listenAll
, waxwane
, toggle
, setWindowGrabbed
, updateOnlyOneSetting
) where

import Prelude hiding (lookup)
import Control.Monad (void, when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT)
import Data.Map (Map, adjust, fromList, lookup)
import Data.Maybe (fromMaybe)

import SDL (LocationMode (AbsoluteLocation, RelativeLocation), GLContext)
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
  , pollEvents
  , setMouseLocationMode
  , ticks
  , windowGrab
  , windowSize
  )

import FastenShade
import Matrix (RenderView (..), fromAffine, fromTranslation)
import Mean (full, weep, doBoth, preent)
import Time (Timewit, beginTime, keepTime)

import Happen (Mousewit (Mousewit), unwrapHappenPointer, unwrapHappenWheel, unwrapHappenWindow)
import Key (Keyset, keyBegun, listen, unkeys)

import MothSpell (mothify)
import Shade (Mesh (meshAnimation), makeAsset, makeAssetMesh, makeSimpleMesh, setMeshTransform)
import Skeleton (evermore, makeAnimation, play)
import Spell (summon, unwrappingly)
import Stavemake (Staveware, makeFeather)


type Settings = Map Setting Bool
data Setting = ShowTicks | ShowKeys | RunTests deriving (Show, Eq, Ord)

makeSettings :: Settings
makeSettings = fromList $ map (, False) [ShowTicks, ShowKeys, RunTests]

toggle :: Setting -> Settings -> Settings
toggle = adjust not

data Allwit = Allwit {
  settings :: Settings
, events :: [SDL.Event]
, keyset :: Keyset
, mouse :: Mousewit
, timewit :: Timewit
, window :: SDL.Window
, context :: SDL.GLContext
, staveware :: Staveware
, display :: RenderView
}

makeAllwit :: Float -> SDL.Window -> SDL.GLContext -> Staveware -> RenderView -> Allwit
makeAllwit ticks = Allwit
  makeSettings
  []
  unkeys
  (Mousewit (Vertex2 0 0) (Vertex2 0 0))
  (beginTime ticks)

begetMeshes :: Float -> IO (Staveware, [Mesh])
begetMeshes now = do
  cocoon <- summon "assets/bunny.moth"
  let mothFile = unwrappingly mothify cocoon

  bun <- makeAssetMesh $ makeAsset "bunny"
  let bunAnimation = (play now . evermore) (makeAnimation mothFile) BUNNY_IDLE
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

listenEvents :: StateT Allwit IO ()
listenEvents = do
  allwit <- get
  es <- lift SDL.pollEvents
  put allwit { events = es }

listenTime :: StateT Allwit IO ()
listenTime = do
  allwit <- get
  now <- lift $ fromIntegral <$> SDL.ticks
  put allwit { timewit = keepTime (timewit allwit) now }

listenKeys :: StateT Allwit IO ()
listenKeys = do
  allwit <- get
  put allwit { keyset = listen (events allwit) (keyset allwit) }

listenMouse :: StateT Allwit IO ()
listenMouse = do
  allwit <- get
  -- todo sum instead of head
  let (p, w) = doBoth unwrapHappenPointer unwrapHappenWheel (events allwit)
  -- todo: this is horrific
  -- todo: also it doesnt work lol
  put allwit { mouse = let f x = if full x then head x else Vertex2 0 0 in Mousewit (f p) (f w) }

listenWindow :: StateT Allwit IO ()
listenWindow = do
  allwit <- get
  when (or . unwrapHappenWindow $ events allwit) $ do
    dis <- lift (waxwane $ window allwit)
    put allwit { display = dis }

updateAllSettings :: StateT Allwit IO ()
updateAllSettings = do
  allwit <- get
  when (keyBegun (keyset allwit) ScancodeK) (updateOnlyOneSetting ShowKeys)
  when (keyBegun (keyset allwit) ScancodeT) (updateOnlyOneSetting ShowTicks)

updateOnlyOneSetting :: Setting -> StateT Allwit IO ()
updateOnlyOneSetting setting = do
  allwit <- get
  put allwit { settings = toggle setting (settings allwit) }

listenAll :: StateT Allwit IO ()
listenAll = do
  allwit <- get
  listenEvents
  listenTime
  listenKeys
  listenMouse
  listenWindow
  updateAllSettings
  when (fromMaybe False $ lookup ShowKeys $ settings allwit) (preent $ keyset allwit)
  when (fromMaybe False $ lookup ShowTicks $ settings allwit) (preent $ timewit allwit)

fand :: Allwit -> IO ()
fand = ($ weep) . when . fromMaybe False . lookup RunTests . settings

setWindowGrabbed :: Bool -> StateT Allwit IO ()
setWindowGrabbed setting = do
  allwit <- get
  SDL.windowGrab (window allwit) $= setting
  void $ SDL.setMouseLocationMode $ if setting then RelativeLocation else AbsoluteLocation

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
