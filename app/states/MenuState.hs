module MenuState (
  MenuState (..)
, makeMenuState
) where

import Numeric.LinearAlgebra (ident)
import Control.Monad.State (MonadState (get, put), StateT, MonadTrans (lift))

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (MenuName, PlayName), Stately (..))

import Blee (bg, darkwhelk, lightwhelk, Blee, red, blue)
import Key (KeySet, keyBegun)
import Matrix (getOrthographicMatrix, getPerspectiveMatrix, RenderView (size))
import Shade (drawMesh)
import Stavemake (Staveware)
import Rime ((*^))
import Statework (stavewrite)


data MenuState = MenuState {
  hand :: [(StateName, String)]
, finger :: Int
, choosen :: Maybe StateName
, _staveware :: Staveware
}

instance Stately MenuState where
  name _ = MenuName
  staveware = _staveware
  update (keyset, _, _, _, _) = do
    _ <- get
    menuFare keyset

  render (_, _, _, display, time) = do
    statewit <- get
    bg darkwhelk
    lift $ drawMesh
      (getPerspectiveMatrix display)
      (ident 4)
      (getOrthographicMatrix display)
      time
      (snd $ staveware statewit)

    let (width, height) = size display
    stavewrite ((1/8) *^ Vertex2 width (height*4)) 1 lightwhelk "WƐLKƏM TU FRⱰGFƆRD!"
    stavewrite ((1/8) *^ Vertex2 width (height*3)) 1 (whelken statewit 0) "plej"
    stavewrite ((1/8) *^ Vertex2 width (height*2)) 1 (whelken statewit 1) "frɒg"
    stavewrite ((1/8) *^ Vertex2 width  height   ) 1 (whelken statewit 2) "towd"

whelken :: MenuState -> Int -> Blee
whelken statewit n = if mod (finger statewit) (length $ hand statewit) == n then red else blue

makeMenuState :: Staveware -> MenuState
makeMenuState ware = MenuState {
  hand = [(PlayName, "play"), (PlayName, "frog"), (PlayName, "toad")]
, finger = 0
, choosen = Nothing
, _staveware = ware
}

menuFare :: KeySet -> StateT MenuState IO ()
menuFare keyset = do
  menuwit <- get
  if keyBegun keyset ScancodeReturn
  then put menuwit { choosen = Just . fst $ hand menuwit!!finger menuwit }
  else put menuwit {
    finger = if keyBegun keyset ScancodeUp
        then mod (pred $ finger menuwit) (length $ hand menuwit)
      else if keyBegun keyset ScancodeDown
        then mod (succ $ finger menuwit) (length $ hand menuwit)
        else finger menuwit
    }
