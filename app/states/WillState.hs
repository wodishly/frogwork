module WillState (
  WillState (..)
, makeWillState
, chosen
) where

import Control.Lens ((.~))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, execStateT)
import Data.Maybe (fromJust, isJust)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import Allwit (Allwit (keyset), Settings, Setting (..), updateOnlyOneSetting)
import State (StateName (WillName), Stately (..))

import Blee (bg, darkwhelk, lightwhelk, red)
import Key (keyBegun)
import Mean (hit, ssss)
import Rime (Point)
import Stavework (Writing, blee, makeWriting, renderFeather, stavewriteAll)


data WillState = WillState {
  hand :: [Maybe Setting]
, finger :: Int
, settings :: Settings
, writings :: [Writing]
}

instance Show WillState where
  show (WillState _ f s _) = show f ++ show s

instance Stately WillState where
  name _ = WillName

  update allwit = do
    _ <- get
    choosefare allwit

  render allwit = do
    willwit <- get
    bg darkwhelk
    renderFeather allwit
    ws <- stavewriteAll allwit (writings willwit)
    put willwit { writings = ws }

makeWillState :: Point -> Settings -> WillState
makeWillState (Vertex2 w h) sets = WillState {
  hand = [
    Just ShowKeys
  , Just ShowTicks
  , Nothing
  ]
, finger = 0
, settings = sets
, writings = [
    makeWriting (Vertex2 (w/2) (h*3/4)) "WꞮLZ"
  , makeWriting (Vertex2 (w/2) (h*3/7)) "tɛl kiz"
  , makeWriting (Vertex2 (w/2) (h*2/7)) "tɛl tɪks"
  , makeWriting (Vertex2 (w/2) (h  /7)) "bæk"
  ]
}

chosen :: WillState -> Maybe Setting
chosen wit = hand wit!!finger wit

choosefare :: Allwit -> StateT WillState IO Allwit
choosefare allwit = do
  willwit <- get

  nudgeFinger allwit
  showFinger

  if keyBegun (keyset allwit) ScancodeReturn && isJust (chosen willwit)
  then lift $ execStateT (updateOnlyOneSetting $ fromJust $ chosen willwit) allwit
  else return allwit

nudgeFinger :: Allwit -> StateT WillState IO ()
nudgeFinger allwit = do
  willwit <- get
  let nudge
        | keyBegun (keyset allwit) ScancodeUp = pred
        | keyBegun (keyset allwit) ScancodeDown = succ
        | otherwise = id
  put willwit { finger = ssss (mod.nudge.finger) (length.hand) willwit }

showFinger :: StateT WillState IO ()
showFinger = do
  titlewit <- get
  put titlewit {
    writings = hit (succ $ finger titlewit) (blee.~red)
      $ map (blee.~lightwhelk) (writings titlewit)
  }
