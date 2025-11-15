{-# OPTIONS_GHC -Wno-type-defaults #-}
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
import Matrix (RenderView (size))
import Stavework (Writing (blee), makeWriting, renderFeather, stavewrite)
import Mean (hit, preent)


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

makeTitleState :: RenderView -> TitleState
makeTitleState dis = TitleState {
  hand = [PlayName, WillName, EndName]
, finger = 0
, writings = [
    makeWriting "WƐLKƏM TU FRⱰGFƆRD!" (Vertex2 (width/2) (height*3/4))
  , makeWriting "plej" (Vertex2 (width/2) (height*3/7))
  , makeWriting "wɪlz" (Vertex2 (width/2) (height*2/7))
  , makeWriting "ɛnd" (Vertex2 (width/2) (height  /7))
  ]
} where (width, height) = size dis

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
