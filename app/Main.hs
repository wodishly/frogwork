module Main (main) where

import Control.Monad.State (StateT, execStateT)

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL (
    GLContext
  , Window
  , createWindow
  , destroyWindow
  , getKeyboardState
  , glCreateContext
  , glDeleteContext
  , initializeAll
  , quit
  )

import MenuState (makeMenuState)
import PauseState (makePauseState)
import PlayState (makePlayState)

import Game (
    Allwit, makeAllwit
  , fand, updateAll, showLeechwit
  , settleState, blit, again, begetMeshes
  )
import FastenMain (openGLWindow)
import Happen (waxwane)
import Spell (summon, unwrappingly)
import MothSpell (mothify)


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
  (staveware, meshes) <- begetMeshes

  cocoon <- summon "assets/bunny.moth"
  let mothFile = unwrappingly mothify cocoon
  print mothFile

  let allwit = makeAllwit staveware window display context
        (makePlayState staveware meshes)
        (makePauseState staveware)
        (makeMenuState staveware)

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
