module Main where

import Control.Monad.State

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL

import Allwit
import FastenMain
import Frogwork
import Key
import Loudness
import Matrix
import Mean
import Stateteller


main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "frogwork" openGLWindow
  context <- SDL.glCreateContext window
  display <- waxwane window
  loudness <- spoken

  unlockKeys

  SDL.ticks
    >>= birth window context display loudness . fromIntegral
    >>= evalStateT live
    >>= die

birth :: SDL.Window -> SDL.GLContext -> RenderView -> Loudness -> Float -> IO Frogwork
birth window ctx display loudness ticks = do
  (staveware, meshes) <- begetMeshes ticks
  SDL.V2 x y <- (fromIntegral <$>) <$> SDL.get (SDL.windowSize window)

  allwit@Allwit { settings } <- fand $ makeAllwit ticks window ctx staveware display loudness

  return Frogwork {
    allwit,
    stateteller = makeStateteller (x, y) settings meshes
  }

live :: StateT Frogwork IO (SDL.GLContext, SDL.Window, Loudness)
live = do
  Frogwork {
    allwit = Allwit {
      context,
      window,
      loudness
    }
  } <- get
  listen
  choose
  become
  didEnd >>= mif
    (return (context, window, loudness))
    live

die :: (SDL.GLContext, SDL.Window, Loudness) -> IO ()
die (context, window, loudness) = do
  GL.finish
  SDL.glDeleteContext context
  SDL.destroyWindow window
  bestill loudness
  SDL.quit
