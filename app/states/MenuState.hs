module MenuState (
  MenuState
, makeMenuState
, choosen
, hand
, finger
) where

import Control.Lens (makeLenses, (^.))
import Control.Monad.State (MonadState (get, put), StateT, MonadTrans (lift))

import SDL.Input.Keyboard.Codes

import State (StateName (MenuName, PlayName), Stately (..))

import Blee (bg, clerp, white)
import Key (KeySet, keyBegun)
import Stave (Staveware, stavewrite, Staveware)
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))


data MenuState = MenuState {
  _hand :: [(StateName, String)]
, _finger :: Int
, _choosen :: Maybe StateName
, _staveware :: Staveware
}
makeLenses ''MenuState

instance Stately MenuState where
  name _ = MenuName
  update (keyset, _, _, _, _) = do
    _ <- get
    menuFare keyset

  render _ = do
    statewit <- get
    bg (clerp (1/4) white)
    lift $ stavewrite (statewit^.staveware) (Vertex2 -100 0) 1 "FROG"

makeMenuState :: Staveware -> MenuState
makeMenuState ware = MenuState {
  _hand = [(PlayName, "play"), (PlayName, "frog")]
, _finger = 0
, _choosen = Nothing
, _staveware = ware
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
