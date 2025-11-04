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
  _placeholder :: ()
}
makeLenses ''MenuState

type MenuUpdate = News -> StateT MenuState IO (Maybe StateName)

instance Stately MenuState where
  _name _ = Menu

makeMenuState :: MenuState
makeMenuState = MenuState {
  _hand = [(Play, "play"), (Play, "frog")],
  _finger = 0,
  _placeholder = ()
}

menuState :: News -> StateT MenuState IO (Maybe StateName)
menuState (_, keyset, _, _) = do
  menuwit <- get
  lift $ bg (clerp (1/4) white)
  lift $ print $ (menuwit^.hand)!!(menuwit^.finger)
  settleState keyset

settleState :: KeySet -> StateT MenuState IO (Maybe StateName)
settleState keyset = do
  menuwit <- get
  if keyBegun keyset ScancodeReturn
    then return $ Just $ fst $ (menuwit^.hand)!!(menuwit^.finger)
  else do
    put $ menuwit {
      _finger = if keyBegun keyset ScancodeUp
          then mod (succ $ menuwit^.finger) (length $ menuwit^.hand)
        else if keyBegun keyset ScancodeDown
          then mod (pred $ menuwit^.finger) (length $ menuwit^.hand)
        else menuwit^.finger
    }
    return Nothing
