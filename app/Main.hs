module Main (main) where

import Control.Monad.State (MonadState (get), StateT, evalStateT)

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
  ( GLContext,
    V2 (V2),
    Window,
    createWindow,
    destroyWindow,
    get,
    glCreateContext,
    glDeleteContext,
    initializeAll,
    quit,
    ticks,
    windowSize,
  )

import Allwit (Allwit (..), begetMeshes, fand, makeAllwit, window)
import Frogwork (Frogwork (..), didEnd, listen, choose, waxwane, become)
import Stateteller (makeStateteller)

import FastenMain (openGLWindow)
import Loudness (Loudness, spoken, bestill)
import Matrix (RenderView)
import Key (unlockKeys)


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
birth wind ctx display loudness ticks = do
  (staveware, meshes) <- begetMeshes ticks
  SDL.V2 x y <- (fromIntegral <$>) <$> SDL.get (SDL.windowSize wind)

  allwit@Allwit { settings } <- fand $ makeAllwit ticks wind ctx staveware display loudness

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
  didEnd >>= \x -> if x
  then return (context, window, loudness)
  else live

die :: (SDL.GLContext, SDL.Window, Loudness) -> IO ()
die (context, window, loudness) = do
  GL.finish
  SDL.glDeleteContext context
  SDL.destroyWindow window
  bestill loudness
  SDL.quit
