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
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (MenuName, PlayName), Stately (..))

import Blee (bg, darkwhelk)
import Key (KeySet, keyBegun)
import Matrix (_size, getOrthographicMatrix, getPerspectiveMatrix)
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

  render (_, _, _, display, time) = do
    statewit <- get
    bg darkwhelk
    lift $ drawMesh
      (getPerspectiveMatrix display)
      (ident 4)
      (getOrthographicMatrix display)
      time
      (snd $ statewit^.staveware)

    let (width, height) = _size display
    lift $ stavewrite (statewit^.staveware) (Vertex2 (width/8) (height*4/8)) 1 "welcome to frogford!"
    lift $ stavewrite (statewit^.staveware) (Vertex2 (width/8) (height*3/8)) 1 "play"
    lift $ stavewrite (statewit^.staveware) (Vertex2 (width/8) (height*2/8)) 1 "quit"

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
