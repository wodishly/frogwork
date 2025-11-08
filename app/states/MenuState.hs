module MenuState (
  MenuState
, makeMenuState
, choosen
, hand
, finger
) where

import Control.Lens (makeLenses, (^.))
import Control.Monad.State (MonadState (get, put), StateT)

import SDL.Input.Keyboard.Codes

import FrogState (News, StateName (..), Stately (..))

import Key (KeySet, keyBegun)
import Blee (bg, clerp, white)


data MenuState = MenuState {
  _hand :: [(StateName, String)]
, _finger :: Int
, _choosen :: Maybe StateName
} deriving (Show, Eq)
makeLenses ''MenuState

instance Stately MenuState where
  _name _ = MenuName
  _update = menu

makeMenuState :: MenuState
makeMenuState = MenuState {
  _hand = [(PlayName, "play"), (PlayName, "frog")]
, _finger = 0
, _choosen = Nothing
}

menu :: News -> StateT MenuState IO ()
menu (keyset, _, _, _) = do
  _ <- get
  bg (clerp (1/4) white)
  menuFare keyset

menuFare :: KeySet -> StateT MenuState IO ()
menuFare keyset = do
  menuwit <- get
  if keyBegun keyset ScancodeReturn
  then put menuwit { _choosen = Just . fst $ (menuwit^.hand)!!(menuwit^.finger) }
  else put menuwit {
    _finger = if keyBegun keyset ScancodeUp
        then mod (succ $ menuwit^.finger) (length $ menuwit^.hand)
      else if keyBegun keyset ScancodeDown
        then mod (pred $ menuwit^.finger) (length $ menuwit^.hand)
        else menuwit^.finger
    }
