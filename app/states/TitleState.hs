module TitleState (
  TitleState (..)
, makeTitleState
) where

import Control.Monad.State (MonadState (get, put), StateT)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (TitleName, PlayName), Stately (..))

import Blee (Blee, bg, blue, darkwhelk, lightwhelk, red)
import Key (Keyset, keyBegun)
import Matrix (RenderView (size))
import Stavework (Stake (..), stavewrite, renderFeather)
import Stavemake (Staveware)


data TitleState = TitleState {
  hand :: [(StateName, String)]
, finger :: Int
, choosen :: Maybe StateName
, _staveware :: Staveware
}

instance Stately TitleState where
  name _ = TitleName
  staveware = _staveware
  update (keyset, _, _, _) = do
    _ <- get
    titleFare keyset

  render (_, _, display, time) = do
    statewit <- get
    bg darkwhelk
    renderFeather display time (staveware statewit)

    let (width, height) = size display
    stavewrite (Vertex2 (width/2) (height*3/4)) (Middle, Middle) (Vertex2 1 1) lightwhelk "WƐLKƏM TU FRⱰGFƆRD!"
    stavewrite (Vertex2 (width/2) (height*3/7)) (Middle, Middle) (Vertex2 1 1) (whelken statewit 0) "plej"
    stavewrite (Vertex2 (width/2) (height*2/7)) (Middle, Middle) (Vertex2 1 1) (whelken statewit 1) "frɒg"
    stavewrite (Vertex2 (width/2) (height  /7)) (Middle, Middle) (Vertex2 1 1) (whelken statewit 2) "towd"

whelken :: TitleState -> Int -> Blee
whelken statewit n = if mod (finger statewit) (length $ hand statewit) == n then red else blue

makeTitleState :: Staveware -> TitleState
makeTitleState ware = TitleState {
  hand = [(PlayName, "play"), (PlayName, "frog"), (PlayName, "toad")]
, finger = 0
, choosen = Nothing
, _staveware = ware
}

titleFare :: Keyset -> StateT TitleState IO ()
titleFare keyset = do
  titlewit <- get
  if keyBegun keyset ScancodeReturn
  then put titlewit { choosen = Just . fst $ hand titlewit!!finger titlewit }
  else put titlewit {
    finger = if keyBegun keyset ScancodeUp
        then mod (pred $ finger titlewit) (length $ hand titlewit)
      else if keyBegun keyset ScancodeDown
        then mod (succ $ finger titlewit) (length $ hand titlewit)
        else finger titlewit
    }
