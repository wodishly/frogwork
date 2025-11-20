module Allwit (
  Allwit (..),
  Setting (..),
  Settings,
  UnholyMeshMash,
  begetMeshes,
  fand,
  answer,
  makeAllwit,
  setWindowGrabbed,
  toggle,
  updateOnlyOneSetting,
  wakeState
) where

import Prelude hiding (lookup)

import Control.Monad (forM_, void, when)
import Control.Monad.State (MonadState (get, put), StateT)
import Data.Map (Map, adjust, fromList, lookup)
import Data.Maybe (fromMaybe)

import Graphics.Rendering.OpenGL (Vertex2 (Vertex2), ($=))
import SDL (GLContext, LocationMode (AbsoluteLocation, RelativeLocation))
import SDL.Input.Keyboard.Codes

import qualified SDL (Event, Window, setMouseLocationMode, windowGrab)

import FastenShade
import Happen (Mousewit (Mousewit))
import Key (Keyset, keyBegun, unkeys)
import Matrix (RenderView (..), fromAffine, fromTranslation)
import Mean (preent, weep)
import MothSpell (mothify)
import Shade (Mesh (meshAnimation), makeAsset, makeAssetMesh, makeSimpleMesh, setMeshTransform)
import Skeleton (evermore, makeAnimation, play)
import Spell (summon, unwrappingly)
import Stavemake (Staveware, makeFeather)
import Time (Timewit (delta), beginTime)


data Setting = ShowTicks | ShowKeys | ShowSpeech | RunTests deriving (Show, Eq, Ord)

type Settings = Map Setting Bool

makeSettings :: Settings
makeSettings = fromList $ map (, False) [ShowTicks, ShowKeys, ShowSpeech, RunTests]

toggle :: Setting -> Settings -> Settings
toggle = adjust not

data Allwit = Allwit {
  settings :: Settings,
  events :: [SDL.Event],
  keyset :: Keyset,
  mouse :: Mousewit,
  timewit :: Timewit,
  window :: SDL.Window,
  context :: SDL.GLContext,
  staveware :: Staveware,
  display :: RenderView
}

makeAllwit :: Float -> SDL.Window -> SDL.GLContext -> Staveware -> RenderView -> Allwit
makeAllwit ticks = Allwit
  makeSettings
  []
  unkeys
  (Mousewit (Vertex2 0 0) (Vertex2 0 0))
  (beginTime ticks)

-- | the frog, the speech, and the rest
type UnholyMeshMash = (Mesh, Mesh, [Mesh])

begetMeshes :: Float -> IO (Staveware, UnholyMeshMash)
begetMeshes now = do
  cocoon <- summon "assets/bunny.moth"
  let mothFile = unwrappingly mothify cocoon

  bun <- makeAssetMesh $ makeAsset "bunny"
  let bunAnimation = (play now . evermore) (makeAnimation mothFile) BUNNY_IDLE
  froggy <- setMeshTransform (fromAffine [1.0, 1.0, 1.0] [0, 0, 0]) $
    bun { meshAnimation = Just bunAnimation }

  earth <- makeSimpleMesh defaultSimpleMeshProfile

  farsee <- setMeshTransform (fromTranslation [-2, 1, 2])
    =<< makeAssetMesh (makeAsset "tv")

  x <- makeFeather "noto-sans"
  hack <- makeSimpleMesh staveMeshProfile
  speech <- makeSimpleMesh speechMeshProfile

  return ((x, hack), (froggy, speech, [earth, farsee, hack]))

updateAllSettings :: StateT Allwit IO ()
updateAllSettings = do
  Allwit { keyset } <- get
  forM_ [(ScancodeK, ShowKeys), (ScancodeT, ShowTicks), (ScancodeTab, ShowSpeech)]
    (\(code, setting) -> when (keyBegun keyset code) (updateOnlyOneSetting setting))

updateOnlyOneSetting :: Setting -> StateT Allwit IO ()
updateOnlyOneSetting setting = do
  allwit@(Allwit { settings }) <- get
  put allwit { settings = toggle setting settings }

answer :: StateT Allwit IO ()
answer = do
  Allwit { settings, keyset, timewit } <- get
  updateAllSettings
  when (fromMaybe False $ lookup ShowKeys settings) (preent keyset)
  when (fromMaybe False $ lookup ShowTicks settings) (preent $ show (1/delta timewit) ++ show timewit)

fand :: Allwit -> IO ()
fand = ($ weep) . when . fromMaybe False . lookup RunTests . settings

setWindowGrabbed :: Bool -> StateT Allwit IO ()
setWindowGrabbed setting = do
  Allwit { window } <- get

  SDL.windowGrab window $= setting
  void $ SDL.setMouseLocationMode $ if setting then RelativeLocation else AbsoluteLocation

wakeState :: Bool -> StateT Allwit IO ()
wakeState grabbed = do
  setWindowGrabbed grabbed
  flushKeys

-- reset keyset on state switch
flushKeys :: StateT Allwit IO ()
flushKeys = do
  allwit <- get
  put allwit { keyset = unkeys }
