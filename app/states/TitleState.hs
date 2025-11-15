{-# OPTIONS_GHC -Wno-type-defaults #-}
module TitleState (
  TitleState (..)
, makeTitleState
) where

import Control.Monad.State (MonadState (get, put), StateT)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (EndName, PlayName, TitleName, WillName), Stately (..), doAt, preent)

import Blee (bg, darkwhelk, red, lightwhelk)
import Key (Keyset, keyBegun)
import Matrix (RenderView (size))
import Stavemake (Staveware)
import Stavework (Writing (blee), makeWriting, renderFeather, stavewrite)
import Mean (hit)


data TitleState = TitleState {
  hand :: [StateName]
, finger :: Int
, choosen :: Maybe StateName
, _staveware :: Staveware
, writings :: [Writing]
}

instance Stately TitleState where
  name _ = TitleName
  staveware = _staveware

  update (keyset, _, _, time) = do
    _ <- get
    doAt time 4000 $ preent "hi"
    choosefare keyset

  render news@(_, _, display, time) = do
    titlewit <- get
    bg darkwhelk
    renderFeather display time (staveware titlewit)

    stavewrite news (writings titlewit)

makeTitleState :: RenderView -> Staveware -> TitleState
makeTitleState dis ware = TitleState {
  hand = [PlayName, WillName, EndName]
, finger = 0
, choosen = Nothing
, _staveware = ware
, writings = [
    makeWriting "WƐLKƏM TU FRⱰGFƆRD!" (Vertex2 (width/2) (height*3/4))
  , makeWriting "plej" (Vertex2 (width/2) (height*3/7))
  , makeWriting "wɪlz" (Vertex2 (width/2) (height*2/7))
  , makeWriting "ɛnd" (Vertex2 (width/2) (height  /7))
  ]
} where (width, height) = size dis

choosefare :: Keyset -> StateT TitleState IO ()
choosefare keyset = do
  titlewit <- get
  put $ if keyBegun keyset ScancodeReturn
  then titlewit { choosen = Just $ hand titlewit!!finger titlewit }
  else let
    finger'
      | keyBegun keyset ScancodeUp = pred $ finger titlewit
      | keyBegun keyset ScancodeDown = succ $ finger titlewit
      | otherwise = finger titlewit
    in titlewit {
      finger = mod finger' (length $ hand titlewit)
    , writings =
        hit (succ $ mod finger' (length $ hand titlewit)) (\w -> w { blee = red })
        $ map (\w -> w { blee = lightwhelk })
        $ writings titlewit
    }
