module Main (main) where

import Control.Monad.State (StateT, execStateT)

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL (
    createWindow
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
    Allwit
  , Overwindow
  , again
  , begetMeshes
  , blit
  , fand
  , makeAllwit
  , settleState
  , showLeechwit
  , updateAll
  , waxwane
  )
import FastenMain (openGLWindow)
import Matrix (RenderView)
import Shade (Mesh)
import Stavemake (Staveware)


main :: IO ()
main = do
  SDL.initializeAll
  _ <- SDL.getKeyboardState
  window <- SDL.createWindow "frogwork" openGLWindow
  context <- SDL.glCreateContext window
  display <- waxwane window

  begetMeshes
    >>= birth (window, context) display
    >>= execStateT live
    >> die (window, context)

birth :: Overwindow -> RenderView -> (Staveware, [Mesh]) -> IO Allwit
birth overwindow display (staveware, meshes) = do

  let allwit = makeAllwit overwindow display staveware
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

die :: Overwindow -> IO ()
die (window, context) = do
  GL.finish
  SDL.glDeleteContext context
  SDL.destroyWindow window
  SDL.quit
