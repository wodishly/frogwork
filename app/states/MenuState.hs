module MenuState (
  MenuState
, makeMenuState
, choosen
, hand
, finger
) where

import Control.Lens (makeLenses, (^.))
import Numeric.LinearAlgebra (ident)
import Control.Monad.State (MonadState (get, put), StateT, MonadTrans (lift))

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2), Color4 (Color4))

import State (StateName (MenuName, PlayName), Stately (..))

import Blee (bg, clerp, white)
import Key (KeySet, keyBegun)
import Matrix (getOrthographicMatrix, getPerspectiveMatrix)
import Shade (drawMesh)
import Stave (Staveware, stavewrite)


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

  render (_, _, _, display, _) = do
    statewit <- get
    bg white
    lift $ drawMesh
      (getPerspectiveMatrix display)
      (ident 4)
      (getOrthographicMatrix display)
      (snd $ statewit^.staveware)
    lift $ stavewrite (statewit^.staveware) (Vertex2 100 100) 1 "Welcome to Frogton."

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
