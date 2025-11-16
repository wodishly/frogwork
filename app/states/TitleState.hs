module TitleState (
  TitleState (..)
, makeTitleState
, chosen
) where

import Control.Monad.State (MonadState (get, put), StateT)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import Allwit (Allwit(..))
import State (StateName (EndName, PlayName, TitleName, WillName), Stately (..), doAt)

import Blee (bg, darkwhelk, red, lightwhelk)
import Key (keyBegun)
import Stavework (Writing (blee), makeWriting, renderFeather, stavewrite)
import Mean (hit, preent)
import Rime (Point)


data TitleState = TitleState {
  hand :: [StateName]
, finger :: Int
, writings :: [Writing]
}

instance Stately TitleState where
  name _ = TitleName

  update allwit = do
    _ <- get
    doAt (timewit allwit) 4000 $ preent "hi"
    choosefare allwit

  render allwit = do
    titlewit <- get
    bg darkwhelk
    renderFeather allwit
    stavewrite allwit (writings titlewit)

makeTitleState :: Point -> TitleState
makeTitleState (Vertex2 w h) = TitleState {
  hand = [PlayName, WillName, EndName]
, finger = 0
, writings = [
    makeWriting "WƐLKƏM TU FRⱰGFƆRD!" (Vertex2 (w/2) (h*3/4))
  , makeWriting "plej" (Vertex2 (w/2) (h*3/7))
  , makeWriting "wɪlz" (Vertex2 (w/2) (h*2/7))
  , makeWriting "ɛnd"  (Vertex2 (w/2) (h  /7))
  ]
}

chosen :: TitleState -> StateName
chosen wit = hand wit!!finger wit

choosefare :: Allwit -> StateT TitleState IO Allwit
choosefare allwit = do
  titlewit <- get

  let keys = keyset allwit
      finger'
        | keyBegun keys ScancodeUp = pred $ finger titlewit
        | keyBegun keys ScancodeDown = succ $ finger titlewit
        | otherwise = finger titlewit

  put titlewit {
    finger = mod finger' (length $ hand titlewit)
  , writings =
      hit (succ $ mod finger' (length $ hand titlewit)) (\w -> w { blee = red })
      $ map (\w -> w { blee = lightwhelk })
      $ writings titlewit
  }
  return allwit
