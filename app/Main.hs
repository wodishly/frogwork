module Main (main) where

import Control.Monad.State (StateT, execStateT)

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL (
    initializeAll, getKeyboardState, quit
  , Window, createWindow, destroyWindow
  , GLContext, glCreateContext, glDeleteContext
  , LocationMode (RelativeLocation), setMouseLocationMode
  )

import State (StateName (..))
import MenuState (makeMenuState)
import PauseState (makePauseState)
import PlayState (makePlayState)

import Game (
    Allwit (..), makeAllwit
  , fand, updateAll, showLeechwit
  , settleState, blit, again
  )
import FastenMain (openGLWindow)
import Happen (waxwane)
import Shade (begetMeshes)
import Data.Binary.Get (runGet)
import MothSpell (mothify)
import Spell (summon)


main :: IO ()
main = do
  SDL.initializeAll
  _ <- SDL.getKeyboardState
  window <- SDL.createWindow "frogwork" openGLWindow
  context <- SDL.glCreateContext window

  birth window context >>= execStateT live >> die window context

birth :: SDL.Window -> SDL.GLContext -> IO Allwit
birth window context = do
  display <- waxwane window
  meshes <- begetMeshes
  _ <- SDL.setMouseLocationMode SDL.RelativeLocation

  cocoon <- summon "assets/bunny.moth"
  let mothFile = runGet mothify cocoon
  print mothFile

  let allwit = makeAllwit window display context
        (makePlayState meshes)
        makePauseState
        makeMenuState
        MenuName

  fand allwit
  return allwit

live :: StateT Allwit IO ()
live = do
  updateAll
  showLeechwit
  settleState
  blit
  again live

die :: SDL.Window -> SDL.GLContext -> IO ()
die window context = do
  GL.finish
  SDL.glDeleteContext context
  SDL.destroyWindow window
  SDL.quit
