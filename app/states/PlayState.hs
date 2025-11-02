{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PlayState where

import Control.Lens
import Foreign (new)
import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified Graphics.Rendering.OpenGL as GL (get)
import qualified Data.HashMap.Strict as HM
import SDL.Vect
import Rime (cast)
import SDL (windowSize, Window, Event)

import FrogState
import Light
import Key
import Shade
import Control.Monad.State
import Time

type Passables = ([Event], KeySet, Window, Time)

playState :: Passables -> StateT Statewit IO Statewit
playState (events, keys, window, time) = do
  statewit <- get
  move keys time
  liftIO $ bg black

  V2 windowWidth windowHeight <- (cast <$>) <$> GL.get (windowSize window)
  viewport $= (Position 0 0, Size windowWidth windowHeight)
  let aspect = fromIntegral windowWidth / fromIntegral windowHeight :: Float
  let display = RenderView {
    aspect = aspect,
    fov = pi / 4.0,
    near = 0.1,
    far = 100.0
  }
  let projectionMatrix = getProjectionMatrix display

  -- mesh rendering --
  let m = statewit^.meshes
  updatePlayerUniforms
  liftIO $ mapM_ (`drawMesh` projectionMatrix) m
  return statewit

updatePlayerUniforms :: StateT Statewit IO ()
updatePlayerUniforms = do
  statewit <- get
  -- assume player = first mesh
  let player = head (statewit^.meshes)
  liftIO $ useMesh player
  let uniforms = uniformMap player

  -- is `new` appropriate here? 
  lilyPtr <- liftIO $ new (statewit^.lily)
  inputLocation <- liftIO $ uniforms HM.! "u_input2d"
  liftIO $ uniformv inputLocation 1 lilyPtr


move :: KeySet -> Time -> StateT Statewit IO ()
move keys time = do
  statewit <- get
  put $ statewit {
    _lily = liftA2 (+)
      ((* (200 * throttle time)) <$> wayward keys)
      (statewit^.lily)
  }

bg :: FrogColor -> IO ()
bg c = clearColor $= c >> clear [ColorBuffer, DepthBuffer]
