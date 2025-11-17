module Main (main) where

import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), execStateT, StateT)

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL (
    GLContext
  , V2 (V2)
  , Window
  , createWindow
  , destroyWindow
  , get
  , getKeyboardState
  , glCreateContext
  , glDeleteContext
  , glSwapWindow
  , initializeAll
  , quit
  , ticks
  , windowSize
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
  SDL.V2 x y <- (fromIntegral <$>) <$> SDL.get (SDL.windowSize wind)

  let wit = makeAllwit ticks wind ctx staveware display
  tell <- makeStateteller (x, y) (settings wit) meshes

  fand wit
  return (Frogwork wit tell)

live :: StateT Frogwork IO ()
live = do
  frogwork <- get

  allwit' <- lift $ execStateT listenAll (frogwork^.allwit)
  put frogwork { _allwit = allwit' }

  settleState
  blit
  unless (didEnd frogwork) live

blit :: StateT Frogwork IO ()
blit = get >>= SDL.glSwapWindow . window . _allwit

die :: SDL.Window -> SDL.GLContext -> IO ()
die wind ctx = do
  GL.finish
  SDL.glDeleteContext ctx
  SDL.destroyWindow wind
  SDL.quit
