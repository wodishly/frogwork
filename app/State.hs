{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module State where

import SDL hiding (Pause, Play)
import Foreign.C
import Control.Lens
import Control.Monad

import Key
import Random
import Light
import Mean
import World
import Frog
import Stave

data OptionsInfo = OptionsInfo {
  _isShowingTicks :: Bool
, _isShowingKeys :: Bool
, _isRunningTests :: Bool
}
makeLenses ''OptionsInfo

data StateName = Play | Pause | Menu | Quit deriving (Show, Eq)
data StateInfo = StateInfo {
  _seed :: Seed
, _currentState :: StateName
, _states :: [StateName]
, _options :: OptionsInfo
, _position :: V2 CFloat
, _keyset :: KeySet
, _menuFinger :: Int
, _feather :: Feather
}
makeLenses ''StateInfo

type Response = [Event] -> StateInfo -> IO StateInfo
type GameState = Renderer -> KeySet -> Response

defaultOptions :: OptionsInfo
defaultOptions = OptionsInfo {
  _isShowingTicks = False
, _isShowingKeys = True
, _isRunningTests = False
}

defaultState :: StateInfo
defaultState = StateInfo {
  _seed = defaultSeed
, _currentState = Menu
, _states = [Play, Pause, Menu, Quit]
, _options = defaultOptions
, _position = zlessly center
, _keyset = unkeys
, _menuFinger = 0
, _feather = defaultFeather
}

move :: StateInfo -> IO StateInfo
move stateInfo = pure $
  set position ((stateInfo^.position) + (zlessly.wayward) (stateInfo^.keyset)) stateInfo

understand :: KeySet -> Response
understand keys _events stateInfo = do
  stateInfo <- pure $ set keyset keys stateInfo
  stateInfo <- toggleOption ScancodeK isShowingKeys stateInfo
  stateInfo <- toggleOption ScancodeT isShowingTicks stateInfo
  decideState stateInfo

decideState :: StateInfo -> IO StateInfo
decideState stateInfo = case stateInfo^.currentState of
  Menu -> do
    stateInfo <- navigateMenu stateInfo
    return $ if (mod (stateInfo^.menuFinger) 3 /= 2) && keyBegun (stateInfo^.keyset) ScancodeReturn
      then set currentState Play stateInfo
      else stateInfo
  _ -> do
    return $ if keyBegun (stateInfo^.keyset) ScancodeP
      then togglePause stateInfo
      else stateInfo

togglePause :: StateInfo -> StateInfo
togglePause stateInfo = set currentState (case stateInfo^.currentState of
    Play -> Pause
    Pause -> Play
    _ -> error "bad state"
  ) stateInfo

toggleOption :: Scancode -> Lens' OptionsInfo Bool -> StateInfo -> IO StateInfo
toggleOption keycode lens stateInfo = pure $ if keyBegun (stateInfo^.keyset) keycode
  then set (options.lens) (not $ stateInfo^.options.lens) stateInfo
  else stateInfo

stateByName :: StateName -> GameState
stateByName name = case name of
  Play -> playState
  Pause -> pauseState
  Menu -> menuState
  _ -> error "bad state"

playState :: GameState
playState renderer _keys _events stateInfo = do
  stateInfo <- move stateInfo
  bg renderer black
  rendererDrawColor renderer $= green
  fillRect renderer (safeRect (stateInfo^.position) (zlessly $ size frog))
  return stateInfo

pauseState :: GameState
pauseState renderer _keys _events stateInfo = do
  bg renderer blue
  return stateInfo

quitState :: GameState
quitState renderer _keys _events stateInfo = do
  bg renderer black
  return stateInfo

menuState :: GameState
menuState renderer _keys _events stateInfo = do
  bg renderer (clerp (1/4) white)
  forM_ (zip [0..] ["play", "frog", "quit"]) (\(i, choice) -> do
    rendererDrawColor renderer $= if mod (stateInfo^.menuFinger) 3 == i then blue else green
    drawWord renderer (stateInfo^.feather) (V2 100 $ 100*(cast.succ) i) choice)
  return stateInfo

navigateMenu :: StateInfo -> IO StateInfo
navigateMenu stateInfo = do
  let gainfinger = fromEnum $ any (keyBegun (stateInfo^.keyset)) [ScancodeUp, ScancodeDown]
  return $ set menuFinger (stateInfo^.menuFinger + gainfinger) stateInfo

bg :: Renderer -> Color -> IO ()
bg renderer color = do
  rendererDrawColor renderer $= color
  clear renderer