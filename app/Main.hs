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
  , ticks
  )

import Allwit (
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


main :: IO ()
main = do
  SDL.initializeAll
  _ <- SDL.getKeyboardState
  window <- SDL.createWindow "frogwork" openGLWindow
  context <- SDL.glCreateContext window
  display <- waxwane window

  SDL.ticks
    >>= birth (window, context) display . fromIntegral
    >>= execStateT live
    >> die (window, context)

birth :: Overwindow -> RenderView -> Float -> IO Allwit
birth overwindow display ticks = do
  (staveware, meshes) <- begetMeshes ticks
  let allwit = makeAllwit ticks overwindow staveware display meshes
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
