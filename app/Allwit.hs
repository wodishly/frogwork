module Allwit (
  module Allwit,
  module Control.Monad.State
) where

import Prelude hiding (lookup)

import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT), evalStateT, execStateT, runState, execState, State)
import Data.Map (Map, adjust, fromList, (!))

import qualified SDL
  ( Event,
    GLContext,
    LocationMode (AbsoluteLocation, RelativeLocation),
    Window,
    setMouseLocationMode,
    windowGrab,
  )

import FastenShade
import Happen
import Key
import Snailheart
import Matrix hiding ((!), fromList)
import Mean
import MothSpell
import Shade
import Skeleton
import Spell
import Stavemake
import Time
import Weird


data Setting = ShowTicks | ShowKeys | ShowSpeech | RunTests | BeLoud deriving (Show, Eq, Ord)

type Settings = Map Setting Bool

makeSettings :: Settings
makeSettings = fromList $ map (, False) [ShowTicks, ShowKeys, ShowSpeech, RunTests, BeLoud]

toggle :: Setting -> Settings -> Settings
toggle = adjust not

data Allwit = Allwit {
  settings :: Settings,
  seed :: FrogSeed,
  events :: [SDL.Event],
  keyset :: Keyset,
  mouse :: Mousewit,
  timewit :: Timewit,
  window :: SDL.Window,
  context :: SDL.GLContext,
  staveware :: Staveware,
  display :: RenderView,
  loudness :: Loudness
}

makeAllwit :: Float -> SDL.Window -> SDL.GLContext -> Staveware -> RenderView -> Loudness -> Allwit
makeAllwit t = Allwit
  makeSettings
  formseed
  []
  unkeys
  (Mousewit (Vertex2 0 0) (Vertex2 0 0))
  (beginTime t)

begetMeshes :: Float -> IO (Staveware, Meshlist)
begetMeshes now = do
  cocoon <- summon "assets/bunny.moth"
  let mothFile = unwrappingly mothify cocoon
  bun <- makeAssetMesh $ makeAsset "bunny"
  let bunAnimation = (play now . evermore) (makeAnimation mothFile) BUNNY_IDLE
  frogset <- makeMeshset $ setMeshTransform (fromAffine (thrice 1) (thrice 0)) bun { meshAnimation = Just bunAnimation }
  earth <- makeSimpleMesh defaultSimpleMeshProfile
  heaven <- setMeshTransform (fromAffine (thrice 80) (thrice -40))
    <$> makeSimpleMesh (frameMeshProfileOf "heaven")

  uncull

  farseeset <- makeMeshset =<< makeAssetMesh (makeAsset "tv")

  feather <- makeFeather "noto-sans"
  hack <- makeSimpleMesh staveMeshProfile
  speech <- makeSimpleMesh speechMeshProfile

  return ((feather, hack), Meshlist {
      bodies = [frogset, farseeset],
      grimes = (speech, []),
      worldlies = [heaven, earth, hack]
    })

makeMeshset :: Mesh -> IO Meshset
makeMeshset m = Meshset m <$> makeSimpleMesh frameMeshProfile

weird :: Allwit -> (Float, Allwit)
weird = first only . weirds 1

weirds :: Int -> Allwit -> ([Float], Allwit)
weirds n allwit@Allwit { seed } = (ws, allwit { seed = seed' })
  where (ws, seed') = repeatState n next seed

updateAllSettings :: StateT Allwit IO ()
updateAllSettings = do
  Allwit { keyset } <- get
  forM_ [
    (ScancodeK, ShowKeys),
    (ScancodeT, ShowTicks),
    (ScancodeTab, ShowSpeech),
    (ScancodeM, BeLoud)
    ] (\(code, setting) -> when (keyBegun keyset code) (updateOnlyOneSetting setting))

updateOnlyOneSetting :: Setting -> StateT Allwit IO ()
updateOnlyOneSetting setting = do
  allwit@(Allwit { settings, loudness = (wile, _) }) <- get
  lift $ when (setting == BeLoud) $ (if settings!setting then rest else unrest) wile
  put allwit { settings = toggle setting settings }

answer :: StateT Allwit IO ()
answer = do
  Allwit { settings, keyset, timewit } <- get
  updateAllSettings
  when (fromMaybe False $ lookup ShowKeys settings) (preent keyset)
  when (fromMaybe False $ lookup ShowTicks settings) (preent timewit)
  -- when (fromMaybe False $ lookup BeLoud settings) (preent "im loud")

fand :: Allwit -> IO Allwit
fand wit@Allwit { settings } = when (fromMaybe False (lookup RunTests settings)) weep >> return wit

setWindowGrabbed :: Bool -> StateT Allwit IO ()
setWindowGrabbed setting = do
  Allwit { window } <- get

  SDL.windowGrab window $= setting
  void $ SDL.setMouseLocationMode $ if setting then SDL.RelativeLocation else SDL.AbsoluteLocation

wakeState :: Bool -> StateT Allwit IO ()
wakeState grabbed = do
  setWindowGrabbed grabbed
  flushKeys

-- reset keyset on state switch
flushKeys :: StateT Allwit IO ()
flushKeys = do
  allwit <- get
  put allwit { keyset = unkeys }
