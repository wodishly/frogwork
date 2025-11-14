module WillState (
  WillState (..)
, makeWillState
, unchoose
) where

import Control.Monad.State (MonadState (get, put), StateT)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (Settings, StateName (WillName, TitleName), Stately (..), isShowingKeys, isShowingTicks, toggle)

import Blee (Blee, bg, blue, darkwhelk, lightwhelk, red)
import Key (Keyset, keyBegun)
import Matrix (RenderView (size))
import Mean (ssss)
import Stavemake (Staveware)
import Stavework (Stake (..), renderFeather, stavewrite)
import Data.Maybe (fromMaybe)


data WillState = WillState {
  hand :: [Either StateName (Settings -> Settings)]
, finger :: Int
, choosen :: Maybe (Either StateName (Settings -> Settings))
, _staveware :: Staveware
, settings :: Settings
}

instance Show WillState where
  show (WillState _ f _ _ s) = show f ++ show s

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
    stavewrite (Vertex2 (width/2) (height*3/4)) (Middle, Middle) (Vertex2 1 1) lightwhelk "WꞮLZ"
    stavewrite (Vertex2 (width/2) (height*3/7)) (Middle, Middle) (Vertex2 1 1) (choosewhelk statewit 0) "tɛl kiz"
    stavewrite (Vertex2 (width/2) (height*2/7)) (Middle, Middle) (Vertex2 1 1) (choosewhelk statewit 1) "tɛl tɪks"
    stavewrite (Vertex2 (width/2) (height  /7)) (Middle, Middle) (Vertex2 1 1) (choosewhelk statewit 2) "bæk"

choosewhelk :: WillState -> Int -> Blee
choosewhelk statewit n = if ssss (mod.finger) (length.hand) statewit == n then red else blue

makeWillState :: Staveware -> Settings -> WillState
makeWillState ware sets = WillState {
  hand = [
    Right $ toggle isShowingKeys
  , Right $ toggle isShowingTicks
  , Left TitleName
  ]
, finger = 0
, choosen = Nothing
, _staveware = ware
, settings = sets
}

unchoose :: Maybe Settings -> StateT WillState IO ()
unchoose sets = do
  willwit <- get
  put willwit {
      settings = fromMaybe (settings willwit) sets
    , choosen = Nothing
  }

choosefare :: Keyset -> StateT WillState IO ()
choosefare keyset = do
  willwit <- get
  put $ if keyBegun keyset ScancodeReturn
  then willwit { choosen = Just $ ssss ((!!) . hand) finger willwit }
  else willwit {
    finger = if keyBegun keyset ScancodeUp
        then ssss (mod.pred.finger) (length.hand) willwit
      else if keyBegun keyset ScancodeDown
        then ssss (mod.succ.finger) (length.hand) willwit
        else finger willwit
    }
