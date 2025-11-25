module WillState (
  WillState (..),
  makeWillState,
  chosen,
  writings
) where

import Control.Lens (makeLenses, (.~))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, execStateT)
import Data.Maybe (fromJust, isJust)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import Allwit (Allwit (Allwit, keyset), Setting (..), Settings, updateOnlyOneSetting)
import State (StateName (WillName), Stately (..))

import Blee (bg, darkwhelk, lightwhelk, red)
import Key (keyBegun)
import Mean (hit)
import Rime (Point)
import Stavework (Writing, blee, makeWriting, renderFeather, stavewriteAll)


data WillState = WillState {
  hand :: [Maybe Setting],
  finger :: Int,
  settings :: Settings,
  _writings :: [Writing]
}
makeLenses ''WillState

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
    ws <- stavewriteAll allwit (_writings willwit)
    put willwit { _writings = ws }
    return allwit

makeWillState :: Point -> Settings -> WillState
makeWillState (Vertex2 w h) sets = WillState {
  hand = [
    Just ShowKeys,
    Just ShowTicks,
    Nothing
  ],
  finger = 0,
  settings = sets,
  _writings = [
    makeWriting (Vertex2 (w/2) (h*3/4)) "WꞮLZ",
    makeWriting (Vertex2 (w/2) (h*3/7)) "tɛl kiz",
    makeWriting (Vertex2 (w/2) (h*2/7)) "tɛl tɪks",
    makeWriting (Vertex2 (w/2) (h  /7)) "bæk"
  ]
}

chosen :: WillState -> Maybe Setting
chosen (WillState { hand, finger }) = hand!!finger

choosefare :: Allwit -> StateT WillState IO Allwit
choosefare allwit@Allwit { keyset } = do
  willwit <- get

  nudgeFinger allwit
  showFinger

  if keyBegun keyset ScancodeReturn && isJust (chosen willwit)
  then lift $ execStateT (updateOnlyOneSetting $ fromJust $ chosen willwit) allwit
  else return allwit

nudgeFinger :: Allwit -> StateT WillState IO ()
nudgeFinger (Allwit { keyset }) = do
  willwit@WillState { hand, finger } <- get
  let nudge
        | keyBegun keyset ScancodeUp = pred
        | keyBegun keyset ScancodeDown = succ
        | otherwise = id
  put willwit { finger = mod (nudge finger) (length hand) }

showFinger :: StateT WillState IO ()
showFinger = do
  willwit@WillState { finger, _writings } <- get
  put willwit {
    _writings = hit (blee.~red) (succ finger)
      $ map (blee.~lightwhelk) _writings
  }
