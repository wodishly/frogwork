module Main (main) where

import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), execStateT, StateT)

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL (
    GLContext
  , Window
  , createWindow
  , destroyWindow
  , getKeyboardState
  , glCreateContext
  , glDeleteContext
  , glSwapWindow
  , initializeAll
  , quit
  , ticks
  )

import Allwit (
    Allwit (settings)
  , begetMeshes
  , fand
  , makeAllwit
  , listenAll
  , waxwane
  , window
  
  )
import Stateteller (Frogwork (..), allwit, didEnd, makeStateteller, settleState)

import FastenMain (openGLWindow)
import Matrix (RenderView)


main :: IO ()
main = do
  SDL.initializeAll
  _ <- SDL.getKeyboardState
  wind <- SDL.createWindow "frogwork" openGLWindow
  ctx <- SDL.glCreateContext wind
  dis <- waxwane wind

  SDL.ticks
    >>= birth wind ctx dis . fromIntegral
    >>= execStateT live
    >> die wind ctx

birth :: SDL.Window -> SDL.GLContext -> RenderView -> Float -> IO Frogwork
birth wind ctx display ticks = do
  (staveware, meshes) <- begetMeshes ticks

  let wit = makeAllwit ticks wind ctx staveware display
      tell = makeStateteller display (settings wit) meshes

  fand wit
  return (Frogwork wit tell)

live :: StateT Frogwork IO ()
live = do
  frogwork <- get

  listenings <- lift $ execStateT listenAll (frogwork^.allwit)
  put frogwork { _allwit = listenings }

  settleState
  blit
  unless (didEnd frogwork) live

blit :: StateT Frogwork IO ()
blit = do
  frogwork <- get
  SDL.glSwapWindow (window $ frogwork^.allwit)

die :: SDL.Window -> SDL.GLContext -> IO ()
die wind ctx = do
  GL.finish
  SDL.glDeleteContext ctx
  SDL.destroyWindow wind
  SDL.quit
