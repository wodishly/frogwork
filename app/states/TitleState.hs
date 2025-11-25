module TitleState (
  TitleState (..),
  makeTitleState,
  chosen,
  writings
) where

import Control.Lens (makeLenses, (.~))
import Control.Monad.State (MonadState (get, put), StateT)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import Allwit (Allwit(..))
import State (StateName (EndName, PlayName, TitleName, WillName, AboutName), Stately (..), doOnceAt)

import Blee (bg, darkwhelk, red, lightwhelk)
import Key (keyBegun)
import Stavework (makeWriting, renderFeather, stavewriteAll, Writing (Writing), blee)
import Mean (hit, preent)
import Rime (Point, FrogVertex (onehood), (*^))
import FastenMain (Stake(Middle))


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
    doOnceAt (timewit allwit) 4000 $ preent "hi"
    choosefare allwit
    return allwit

  render allwit = do
    titlewit <- get
    bg darkwhelk
    renderFeather allwit
    ws <- stavewriteAll allwit (_writings titlewit)
    put titlewit { _writings = ws }
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
