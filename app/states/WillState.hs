module WillState (
  WillState (..)
, makeWillState
, unchoose
) where

import Control.Monad.State (MonadState (get, put), StateT)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (Settings, StateName (WillName, TitleName), Stately (..), isShowingKeys, isShowingTicks, toggle)

import Blee (bg, darkwhelk)
import Key (Keyset, keyBegun)
import Matrix (RenderView (size))
import Mean (ssss)
import Stavemake (Staveware)
import Stavework (renderFeather, stavewrite, Writing, makeWriting)
import Data.Maybe (fromMaybe)
import Control.Monad (when)


data WillState = WillState {
  hand :: [Either StateName (Settings -> Settings)]
, finger :: Int
, choosen :: Maybe (Either StateName (Settings -> Settings))
, _staveware :: Staveware
, settings :: Settings
, writings :: [Writing]
}

instance Show WillState where
  show (WillState _ f _ _ s _) = show f ++ show s

instance Stately WillState where
  name _ = WillName
  staveware = _staveware

  update (keyset, _, _, _) = do
    _ <- get
    choosefare keyset

  render news@(_, _, display, time) = do
    willwit <- get
    bg darkwhelk
    renderFeather display time (staveware willwit)
    stavewrite news (writings willwit)

makeWillState :: RenderView -> Staveware -> Settings -> WillState
makeWillState dis ware sets = WillState {
  hand = [
    Right $ toggle isShowingKeys
  , Right $ toggle isShowingTicks
  , Left TitleName
  ]
, finger = 0
, choosen = Nothing
, _staveware = ware
, settings = sets
, writings = [
    makeWriting "WꞮLZ" (Vertex2 (width/2) (height*3/4))
  , makeWriting "tɛl kiz" (Vertex2 (width/2) (height*3/7))
  , makeWriting "tɛl tɪks" (Vertex2 (width/2) (height*2/7))
  , makeWriting "bæk" (Vertex2 (width/2) (height/7))
  ]
} where (width, height) = size dis

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
  if keyBegun keyset ScancodeUp
    then put $ willwit { finger = ssss (mod.pred.finger) (length.hand) willwit }
  else if keyBegun keyset ScancodeDown
    then put $ willwit { finger = ssss (mod.succ.finger) (length.hand) willwit }
  else when (keyBegun keyset ScancodeReturn) $
    put $ willwit { choosen = Just $ ssss ((!!) . hand) finger willwit }
