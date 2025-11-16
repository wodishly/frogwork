module WillState (
  WillState (..)
, makeWillState
, chosen
) where

import Control.Monad.State (MonadState (get, put), StateT, execStateT, MonadTrans (lift))

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import Allwit (Allwit (keyset), Settings, Setting (..), updateOnlyOneSetting)
import State (StateName (WillName), Stately (..))

import Blee (bg, darkwhelk, red, lightwhelk)
import Key (keyBegun)
import Stavework (Writing (blee), makeWriting, renderFeather, stavewrite)
import Mean (hit)
import Rime (Point)


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
    stavewrite allwit (writings willwit)

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
    makeWriting "WꞮLZ"     (Vertex2 (w/2) (h*3/4))
  , makeWriting "tɛl kiz"  (Vertex2 (w/2) (h*3/7))
  , makeWriting "tɛl tɪks" (Vertex2 (w/2) (h*2/7))
  , makeWriting "bæk"      (Vertex2 (w/2) (h  /7))
  ]
}

chosen :: WillState -> Maybe Setting
chosen wit = hand wit!!finger wit

choosefare :: Allwit -> StateT WillState IO Allwit
choosefare allwit = do
  willwit <- get

  let keys = keyset allwit
      finger'
        | keyBegun keys ScancodeUp = pred $ finger willwit
        | keyBegun keys ScancodeDown = succ $ finger willwit
        | otherwise = finger willwit
  put willwit {
    finger = mod finger' (length $ hand willwit)
  , writings =
      hit (succ $ mod finger' (length $ hand willwit)) (\w -> w { blee = red })
      $ map (\w -> w { blee = lightwhelk })
      $ writings willwit
  }

  if keyBegun keys ScancodeReturn
    then maybe (return allwit) (\setting -> lift $ execStateT (updateOnlyOneSetting setting) allwit) (chosen willwit)
    else return allwit
