module MenuState where

import Control.Lens (makeLenses, (^.))
import Control.Monad.State

import SDL.Input.Keyboard.Codes

import Light
import Key
import FrogState

data MenuState = MenuState {
  _hand :: [(StateName, String)],
  _finger :: Int,
  _choosen :: Maybe StateName
}
makeLenses ''MenuState

instance Stately MenuState where
  _name _ = Menu
  _update = menuState

makeMenuState :: MenuState
makeMenuState = MenuState {
  _hand = [(Play, "play"), (Play, "frog")],
  _finger = 0,
  _choosen = Nothing
}

menuState :: News -> StateT MenuState IO ()
menuState (keyset, _, _) = do
  _ <- get
  lift $ bg (clerp (1/4) white)
  menuFare keyset

menuFare :: KeySet -> StateT MenuState IO ()
menuFare keyset = do
  menuwit <- get
  if keyBegun keyset ScancodeReturn
  then do
    put $ menuwit {
      _choosen = Just . fst $ (menuwit^.hand)!!(menuwit^.finger)
    }
  else put $ menuwit {
    _finger = if keyBegun keyset ScancodeUp
        then mod (succ $ menuwit^.finger) (length $ menuwit^.hand)
      else if keyBegun keyset ScancodeDown
        then mod (pred $ menuwit^.finger) (length $ menuwit^.hand)
      else menuwit^.finger
    }
