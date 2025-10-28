{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant return" #-}

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
import Data.Word
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
}

move :: StateInfo -> IO StateInfo
move stateInfo = pure $
  set position ((stateInfo^.position) + (zlessly.wayward) (stateInfo^.keyset)) stateInfo

understand :: KeySet -> Response
understand keys events stateInfo = do
  stateInfo <- pure $ set keyset keys stateInfo
  stateInfo <- toggleOption ScancodeK isShowingKeys stateInfo
  stateInfo <- toggleOption ScancodeT isShowingTicks stateInfo
  stateInfo <- decideState stateInfo
  return stateInfo

decideState :: StateInfo -> IO StateInfo
decideState stateInfo = case stateInfo^.currentState of
  Menu -> do
    stateInfo <- navigateMenu stateInfo
    return $ if even (stateInfo^.menuFinger) && keyBegun (stateInfo^.keyset) ScancodeReturn
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

playState :: GameState
playState renderer keys events stateInfo = do
  stateInfo <- move stateInfo
  bg renderer black
  rendererDrawColor renderer $= green
  fillRect renderer (safeRect (stateInfo^.position) (zlessly $ size frog))
  return stateInfo

pauseState :: GameState
pauseState renderer keys events stateInfo = do
  bg renderer blue
  return stateInfo

menuState :: GameState
menuState renderer keys events stateInfo = do
  bg renderer (clerp (1/4) white)
  rendererDrawColor renderer $= lif (even $ stateInfo^.menuFinger) blue green
  fillRect renderer (safeRect 100 100)
  rendererDrawColor renderer $= lif (odd $ stateInfo^.menuFinger) blue green
  fillRect renderer (safeRect (V2 100 300) 100)
  rendererDrawColor renderer $= yellow
  drawWord renderer 20 50 "abcdefghijklm"
  drawWord renderer (V2 20 130) 100 "nopqrstuvwxyz"
  return stateInfo

navigateMenu :: StateInfo -> IO StateInfo
navigateMenu stateInfo = do
  let gainfinger = fromEnum $ any (keyBegun (stateInfo^.keyset)) [ScancodeUp, ScancodeDown]
  stateInfo <- pure $ set menuFinger (stateInfo^.menuFinger + gainfinger) stateInfo
  return stateInfo

bg :: Renderer -> Color -> IO ()
bg renderer color = do
  rendererDrawColor renderer $= color
  clear renderer