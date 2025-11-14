module WillState (
  WillState (..)
, makeWillState
) where

import Control.Monad.State (MonadState (get, put), StateT)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (WillName), Stately (..))

import Blee (Blee, bg, blue, darkwhelk, lightwhelk, red)
import Key (Keyset, keyBegun)
import Matrix (RenderView (size))
import Stavemake (Staveware)
import Stavework (Stake (..), renderFeather, stavewrite)
import Mean (ssss)


data WillState = WillState {
  hand :: [IO ()]
, finger :: Int
, choosen :: Maybe (IO ())
, _staveware :: Staveware
}

instance Stately WillState where
  name _ = WillName
  staveware = _staveware

  update (keyset, _, _, _) = do
    _ <- get
    choosefare keyset

  render (_, _, display, time) = do
    statewit <- get
    bg darkwhelk
    renderFeather display time (staveware statewit)

    let (width, height) = size display
    stavewrite (Vertex2 (width/2) (height*3/4)) (Middle, Middle) (Vertex2 1 1) lightwhelk "WƐLKƏM TU FRⱰGFƆRD!"
    stavewrite (Vertex2 (width/2) (height*3/7)) (Middle, Middle) (Vertex2 1 1) (choosewhelk statewit 0) "plej"
    stavewrite (Vertex2 (width/2) (height*2/7)) (Middle, Middle) (Vertex2 1 1) (choosewhelk statewit 1) "wɪlz"
    stavewrite (Vertex2 (width/2) (height  /7)) (Middle, Middle) (Vertex2 1 1) (choosewhelk statewit 2) "ɛnd"

choosewhelk :: WillState -> Int -> Blee
choosewhelk statewit n = if ssss (mod.finger) (length.hand) statewit == n then red else blue

makeWillState :: Staveware -> WillState
makeWillState ware = WillState {
  hand = [
--     toggleSetting ScancodeK isShowingKeys
--   , toggleSetting ScancodeT isShowingTicks
  ]
, finger = 0
, choosen = Nothing
, _staveware = ware
}

choosefare :: Keyset -> StateT WillState IO ()
choosefare keyset = do
  titlewit <- get
  put $ if keyBegun keyset ScancodeReturn
  then titlewit { choosen = Just $ ssss ((!!).hand) finger titlewit }
  else titlewit {
    finger = if keyBegun keyset ScancodeUp
        then ssss (mod.pred.finger) (length.hand) titlewit
      else if keyBegun keyset ScancodeDown
        then ssss (mod.succ.finger) (length.hand) titlewit
        else finger titlewit
    }
