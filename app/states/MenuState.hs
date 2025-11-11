module MenuState (
  MenuState (..)
, makeMenuState
) where

import Numeric.LinearAlgebra (ident)
import Control.Monad.State (MonadState (get, put), StateT, MonadTrans (lift))

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))

import State (StateName (MenuName, PlayName), Stately (..))

import Blee (bg, darkwhelk)
import Key (KeySet, keyBegun)
import Matrix (getOrthographicMatrix, getPerspectiveMatrix, RenderView (size))
import Shade (drawMesh)
import Stave (Staveware, stavewrite)


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
    lift $ stavewrite (staveware statewit) (Vertex2 (width/8) (height*4/8)) 1 "welcome to frogford!"
    lift $ stavewrite (staveware statewit) (Vertex2 (width/8) (height*3/8)) 1 "play"
    lift $ stavewrite (staveware statewit) (Vertex2 (width/8) (height*2/8)) 1 "quit"

makeMenuState :: Staveware -> MenuState
makeMenuState ware = MenuState {
  hand = [(PlayName, "play"), (PlayName, "frog")]
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
        then mod (succ $ finger menuwit) (length $ hand menuwit)
      else if keyBegun keyset ScancodeDown
        then mod (pred $ finger menuwit) (length $ hand menuwit)
        else finger menuwit
    }
