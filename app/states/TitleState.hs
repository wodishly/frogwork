module TitleState where

import Allwit
import Blee
import FastenMain
import Key
import Mean
import Rime
import State
import Stavework


data TitleState = TitleState {
  hand :: [StateName],
  finger :: Int,
  _writings :: [Writing]
}
makeLenses ''TitleState

instance Stately TitleState where
  name _ = TitleName

  update allwit = do
    _ <- get
    choosefare allwit
    return allwit

  render allwit = do
    bg darkwhelk
    stavewrite writings allwit
    return allwit

makeTitleState :: Point -> TitleState
makeTitleState (Vertex2 w h) = TitleState {
  hand = [PlayName, WillName, AboutName, EndName],
  finger = 0,
  _writings = [
    makeWriting (Vertex2 (w/2) (h*3/4)) "WƐLKƏM TU FRⱰGFƆRD!",
    Writing (Middle, Middle) ((3/4) *^ onehood) lightwhelk Nothing (Vertex2 (w/2) (h*4/9)) "plej",
    Writing (Middle, Middle) ((3/4) *^ onehood) lightwhelk Nothing (Vertex2 (w/2) (h*3/9)) "wɪlz",
    Writing (Middle, Middle) ((3/4) *^ onehood) lightwhelk Nothing (Vertex2 (w/2) (h*2/9)) "əbawt",
    Writing (Middle, Middle) ((3/4) *^ onehood) lightwhelk Nothing (Vertex2 (w/2) (h  /9)) "ɛnd"
  ]
}

chosen :: TitleState -> StateName
chosen (TitleState { hand, finger }) = hand!!finger

choosefare :: Allwit -> StateT TitleState IO ()
choosefare allwit = do
  nudgeFinger allwit
  showFinger

nudgeFinger :: Allwit -> StateT TitleState IO ()
nudgeFinger (Allwit { keyset }) = do
  titlewit@TitleState { hand, finger } <- get
  let nudge
        | keyBegun keyset ScancodeUp = pred
        | keyBegun keyset ScancodeDown = succ
        | otherwise = id
  put titlewit { finger = mod (nudge finger) (length hand) }

showFinger :: StateT TitleState IO ()
showFinger = do
  titlewit@TitleState { finger, _writings } <- get
  put titlewit {
    _writings = hit (blee.~red) (succ finger)
      $ map (blee.~lightwhelk) _writings
  }
