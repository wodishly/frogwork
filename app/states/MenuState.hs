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

import State (StateName (MenuName, PlayName), Stately (..))

import Blee (bg, clerp, white)
import Key (KeySet, keyBegun)


data MenuState = MenuState {
  _hand :: [(StateName, String)]
, _finger :: Int
, _choosen :: Maybe StateName
} deriving (Show, Eq)
makeLenses ''MenuState

instance Stately MenuState where
  name _ = MenuName
  update (keyset, _, _, _, _) = do
    _ <- get
    menuFare keyset

  render _ = do
    _ <- get
    bg (clerp (1/4) white)


makeMenuState :: MenuState
makeMenuState = MenuState {
  _hand = [(PlayName, "play"), (PlayName, "frog")]
, _finger = 0
, _choosen = Nothing
}

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
