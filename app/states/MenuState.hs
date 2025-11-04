{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module MenuState where

import Control.Lens
import Control.Monad.State

import FrogState
import Light
import Key
import SDL.Input.Keyboard.Codes

data MenuState = MenuState {
  _hand :: [(StateName, String)],
  _finger :: Int,
  _chosen :: Maybe StateName
}
makeLenses ''MenuState

instance Stately MenuState where
  _name _ = Menu
  _update = menuState

makeMenuState :: MenuState
makeMenuState = MenuState {
  _hand = [(Play, "play"), (Play, "frog")],
  _finger = 0,
  _chosen = Nothing
}

menuState :: News -> StateT MenuState IO ()
menuState (_, keyset, _, _) = do
  menuwit <- get
  lift $ bg (clerp (1/4) white)
  menuFare keyset

menuFare :: KeySet -> StateT MenuState IO ()
menuFare keyset = do
  menuwit <- get
  if keyBegun keyset ScancodeReturn
    then put $ menuwit {
      _chosen = Just $ fst $ (menuwit^.hand)!!(menuwit^.finger)
    }
  else do
    put $ menuwit {
      _finger = if keyBegun keyset ScancodeUp
          then mod (succ $ menuwit^.finger) (length $ menuwit^.hand)
        else if keyBegun keyset ScancodeDown
          then mod (pred $ menuwit^.finger) (length $ menuwit^.hand)
        else menuwit^.finger
    }
