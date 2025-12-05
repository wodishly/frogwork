module Main (main) where

import qualified Graphics.Rendering.OpenGL as GL (finish)
import qualified SDL
  ( V2 (V2),
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

import Allwit
import FastenMain
import Frogwork
import Happen
import Key
import Mean
import Snailheart
import Stateteller
import Weird


main :: IO ()
main = do
  SDL.initializeAll

  window <- SDL.createWindow "frogwork" openGLWindow
  context <- SDL.glCreateContext window
  loudness <- spoken
  unlockKeys

  SDL.ticks
    >>= birth (Otherworld window context loudness) . fromIntegral
    >>= evalStateT live
    >>= die

birth :: Otherworld -> Float -> IO Frogwork
birth otherworld@(Otherworld window _ _) ticks = do
  SDL.V2 x y <- (fromIntegral <$>) <$> SDL.get (SDL.windowSize window)
  display <- waxwane window

  (stavebook, meshes) <- begetMeshes ticks
  seed <- formseed

  allwit <- fand $ makeAllwit seed ticks otherworld stavebook display meshes
  return $ uncurry Frogwork (makeStateteller (x, y) allwit)

live :: StateT Frogwork IO Otherworld
live = do
  frogwork <- get
  listen
  choose
  become
  didEnd >>= mif
    (return frogwork.allwit.otherworld)
    live

die :: Otherworld -> IO ()
die (Otherworld window context loudness) = do
  GL.finish
  SDL.glDeleteContext context
  SDL.destroyWindow window
  bestill loudness
  SDL.quit
