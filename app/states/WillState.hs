{-# LANGUAGE FlexibleInstances #-}
module WillState where

import Allwit
import State

import Blee
import Key
import Mean
import Rime
import Stavework


data WillState = WillState {
  _hand :: [Maybe Setting],
  _finger :: Int,
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
    updateHand allwit

  render allwit = do
    bg darkwhelk
    stave writings allwit
    return allwit

instance Choosing WillState (Maybe Setting) where
  chosen wit = _hand wit !! _finger wit

  updateHand allwit@Allwit { keyset } = do
    willwit <- get

    nudgeFinger allwit finger hand
    showFinger writings finger

    if keyBegun keyset ScancodeReturn && isJust (chosen willwit)
    then lift $ execStateT (updateOnlyOneSetting $ fromJust $ chosen willwit) allwit
    else return allwit

makeWillState :: Point -> Settings -> WillState
makeWillState (Vertex2 w h) settings = WillState {
  _hand = [
    Just ShowKeys,
    Just ShowTicks,
    Nothing
  ],
  _finger = 0,
  settings,
  _writings = [
    makeWriting (Vertex2 (w/2) (h*3/4)) "WꞮLZ",
    makeWriting (Vertex2 (w/2) (h*3/7)) "tɛl kiz",
    makeWriting (Vertex2 (w/2) (h*2/7)) "tɛl tɪks",
    makeWriting (Vertex2 (w/2) (h  /7)) "bæk"
  ]
}
