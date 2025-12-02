module TitleState where

import Allwit
import Blee
import FastenMain
import Mean
import Rime
import State
import Stavework


data TitleState = TitleState {
  _hand :: [StateName],
  _finger :: Int,
  _writings :: [Writing]
}
makeLenses ''TitleState

instance Stately TitleState where
  name _ = TitleName

  update allwit = do
    _ <- get
    void $ updateHand allwit
    return allwit

  render allwit = do
    bg darkwhelk
    stave writings allwit
    return allwit

instance Choosing TitleState StateName where
  chosen wit = _hand wit !! _finger wit

  updateHand allwit = do
    nudgeFinger allwit finger hand
    showFinger writings finger
    return allwit

makeTitleState :: Point -> TitleState
makeTitleState (Vertex2 w h) = TitleState {
  _hand = [PlayName, WillName, AboutName, EndName],
  _finger = 0,
  _writings = [
    makeWriting (Vertex2 (w/2) (h*3/4)) "WƐLKƏM TU FRⱰGFƆRD!",
    Writing (Middle, Middle) ((3/4) *^ onehood) lightwhelk Nothing (Vertex2 (w/2) (h*4/9)) "plej",
    Writing (Middle, Middle) ((3/4) *^ onehood) lightwhelk Nothing (Vertex2 (w/2) (h*3/9)) "wɪlz",
    Writing (Middle, Middle) ((3/4) *^ onehood) lightwhelk Nothing (Vertex2 (w/2) (h*2/9)) "əbawt",
    Writing (Middle, Middle) ((3/4) *^ onehood) lightwhelk Nothing (Vertex2 (w/2) (h  /9)) "ɛnd"
  ]
}
