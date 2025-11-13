module MenuState (
  MenuState (..)
, makeMenuState
) where

import Control.Monad.State (MonadState (get, put), StateT)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (MenuName, PlayName), Stately (..))

import Blee (Blee, bg, blue, darkwhelk, lightwhelk, red)
import Key (Keyset, keyBegun)
import Matrix (RenderView (size))
import Stavework (Stake (..), stavewrite, renderFeather)
import Stavemake (Staveware)


data MenuState = MenuState {
  hand :: [(StateName, String)]
, finger :: Int
, choosen :: Maybe StateName
, _staveware :: Staveware
}

instance Stately MenuState where
  name _ = MenuName
  staveware = _staveware
  update (keyset, _, _, _) = do
    _ <- get
    menuFare keyset

  render (_, _, display, time) = do
    statewit <- get
    bg darkwhelk
    renderFeather display time (staveware statewit)

    let (width, height) = size display
    stavewrite (Vertex2 (width/2) (height*3/4)) (Middle, Middle) (Vertex2 1 1) lightwhelk "WƐLKƏM TU FRⱰGFƆRD!"
    stavewrite (Vertex2 (width/2) (height*3/7)) (Middle, Middle) (Vertex2 1 1) (whelken statewit 0) "plej"
    stavewrite (Vertex2 (width/2) (height*2/7)) (Middle, Middle) (Vertex2 1 1) (whelken statewit 1) "frɒg"
    stavewrite (Vertex2 (width/2) (height  /7)) (Middle, Middle) (Vertex2 1 1) (whelken statewit 2) "towd"

whelken :: MenuState -> Int -> Blee
whelken statewit n = if mod (finger statewit) (length $ hand statewit) == n then red else blue

makeMenuState :: Staveware -> MenuState
makeMenuState ware = MenuState {
  hand = [(PlayName, "play"), (PlayName, "frog"), (PlayName, "toad")]
, finger = 0
, choosen = Nothing
, _staveware = ware
}

menuFare :: Keyset -> StateT MenuState IO ()
menuFare keyset = do
  menuwit <- get
  if keyBegun keyset ScancodeReturn
  then put menuwit { choosen = Just . fst $ hand menuwit!!finger menuwit }
  else put menuwit {
    finger = if keyBegun keyset ScancodeUp
        then mod (pred $ finger menuwit) (length $ hand menuwit)
      else if keyBegun keyset ScancodeDown
        then mod (succ $ finger menuwit) (length $ hand menuwit)
        else finger menuwit
    }
