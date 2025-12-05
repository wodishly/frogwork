module Allwit (
  module Allwit,
  module Control.Monad.State
) where

import Prelude hiding (lookup)

import Control.Monad.State (MonadState (get, put), MonadTrans (lift), State, StateT (runStateT), evalStateT, execState, execStateT, runState)
import Data.Map (Map, adjust, fromList, (!))

import qualified SDL
  ( Event,
    LocationMode (AbsoluteLocation, RelativeLocation),
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


data Setting
  = ShowTicks
  | ShowKeys
  | ShowSpeech
  | RunTests
  | BeLoud
  deriving (Show, Eq, Ord)

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
  otherworld :: Otherworld,
  stavebook :: Stavebook,
  display :: RenderView,
  meshhoard :: Meshhoard
}

makeAllwit :: FrogSeed -> Float -> Otherworld -> Stavebook -> RenderView -> Meshhoard -> Allwit
makeAllwit seed t = Allwit
  makeSettings
  seed
  []
  unkeys
  (Mousewit (Vertex2 0 0) (Vertex2 0 0))
  (beginTime t)

begetMeshes :: Float -> IO (Stavebook, Meshhoard)
begetMeshes now = do
  cocoon <- summon "assets/bunny.moth"
  let mothFile = unwrappingly mothify cocoon

  bun <- makeAssetMesh $ makeAsset "bunny"
  let bunAnimation = (play now . evermore) (makeAnimation mothFile) BUNNY_IDLE
  frogset <- makeMeshset $ setMeshTransform (fromAffine (thrice 1) (thrice 0)) bun { meshAnimation = Just bunAnimation }
  farseeset <- makeMeshset =<< makeAssetMesh (makeAsset "tv")

  earth <- makeSimpleMesh defaultSimpleMeshProfile
  heaven <- setMeshTransform (fromAffine (thrice 80) (thrice -40))
    <$> makeSimpleMesh (frameMeshProfileOf "heaven")

  uncull

  feather <- makeFeather "noto-sans"
  stavemesh <- makeSimpleMesh staveMeshProfile
  speech <- makeSimpleMesh speechMeshProfile

  return (feather, Meshhoard {
    spitfuls = [frogset, farseeset],
    spitlesses = [heaven, earth],
    stavemesh,
    grimes = Grimes speech []
  })

makeMeshset :: Mesh -> IO Meshset
makeMeshset m = Meshset m <$> makeSimpleMesh frameMeshProfile

weird :: Allwit -> (Float, Allwit)
weird = first only . weirds 1

weirds :: Int -> Allwit -> ([Float], Allwit)
weirds n allwit@Allwit { seed } = (ws, allwit { seed = seed' })
  where (ws, seed') = repeatState n next seed

weirdwheel :: Allwit -> ((Float, Float), Allwit)
weirdwheel a = let
  (f, a') = weird a
  winkle = 2*pi*f
  in ((cos winkle, sin winkle), a')

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
  allwit@(Allwit { settings, otherworld = Otherworld { loudness = (wile, _) } }) <- get
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
  Allwit { otherworld = Otherworld { window } } <- get

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
