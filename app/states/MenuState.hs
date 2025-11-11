module MenuState (
  MenuState (..)
, makeMenuState
) where

import Numeric.LinearAlgebra (ident)
import Control.Monad.State (MonadState (get, put), StateT, MonadTrans (lift))

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (MenuName, PlayName), Stately (..))

import Blee (bg, darkwhelk, lightwhelk, Blee, red, green)
import Key (KeySet, keyBegun)
import Matrix (getOrthographicMatrix, getPerspectiveMatrix, RenderView (size))
import Shade (drawMesh)
import Stave (Staveware, stavewrite)
import Rime ((*^))


data MenuState = MenuState {
  hand :: [(StateName, String)]
, finger :: Int
, choosen :: Maybe StateName
, staveware :: Staveware
}

instance Stately MenuState where
  name _ = MenuName
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
    lift $ stavewrite (staveware statewit) ((1/8) *^ Vertex2 width (height*4)) 1 lightwhelk "welcome to frogford!"
    lift $ stavewrite (staveware statewit) ((1/8) *^ Vertex2 width (height*3)) 1 (whelken statewit 0) "play"
    lift $ stavewrite (staveware statewit) ((1/8) *^ Vertex2 width (height*2)) 1 (whelken statewit 1) "frog"
    lift $ stavewrite (staveware statewit) ((1/8) *^ Vertex2 width  height   ) 1 (whelken statewit 2) "toad"

whelken :: MenuState -> Int -> Blee
whelken statewit n = if mod (finger statewit) (length $ hand statewit) == n then red else green

makeMenuState :: Staveware -> MenuState
makeMenuState ware = MenuState {
  hand = [(PlayName, "play"), (PlayName, "frog"), (PlayName, "toad")]
, finger = 0
, choosen = Nothing
, staveware = ware
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
