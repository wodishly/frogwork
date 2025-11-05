{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PlayState where

import Control.Lens
import Control.Monad.State

import Foreign (new)
import qualified Graphics.Rendering.OpenGL as GL (get)
import qualified Data.HashMap.Strict as HM
import SDL (windowSize)
import Graphics.Rendering.OpenGL as GL hiding (get)
import SDL.Vect hiding (Point)
import Rime (cast)

import FrogState
import Light
import Key
import Shade
import Time
import Random
import Matrix

data PlayState = MkPlayState {
    _seed :: FrogSeed
  , _meshes :: [Mesh]
  , _lily :: Point
  , _programs :: [(Program, VertexArrayObject)]
  , _camera :: Camera
}
makeLenses ''PlayState

instance Stately PlayState where
  _name _ = Play
  _update = playState

makePlayState :: PlayState
makePlayState = MkPlayState {
    _seed = defaultSeed
  , _meshes = []
  , _lily = Vertex2 0 0
  , _programs = []
  , _camera = Camera { cPosition = frogZero, cTarget = [0, 0, 1] }
}

playState :: News -> StateT PlayState IO ()
playState (keys, window, time) = do
  statewit <- get

  move keys time
  lift $ bg green

  V2 windowWidth windowHeight <- (cast <$>) <$> GL.get (windowSize window)
  viewport $= (Position 0 0, Size windowWidth windowHeight)

  let projectionMatrix = getProjectionMatrix RenderView {
    _aspect = fromIntegral windowWidth / fromIntegral windowHeight,
    _fov = pi / 4.0,
    _near = 0.1,
    _far = 100.0
  }

  let Vertex2 a b = statewit^.lily
  put $ statewit {
    _camera = Camera {
      cTarget=[cos(pi/2+a/10000),-1,sin(pi/2+a/10000)-1+b/10000],
      cPosition=[0,-1,-1+b/10000]
    }
  }
  let c = statewit^.camera
  let viewMatrix = frogLookAt (cPosition c) (cTarget c)
  -- print viewMatrix
  -- let lapl = detLaplace $ unhew viewMatrix
  -- print lapl

  -- mesh rendering --
  lift $ mapM_ (\mesh -> drawMesh mesh projectionMatrix viewMatrix) (statewit^.meshes)

move :: KeySet -> Time -> StateT PlayState IO ()
move keys time = do
  statewit <- get
  put $ statewit {
    _lily = liftA2 (+)
      ((* (200 * throttle time)) <$> wayward keys)
      (statewit^.lily)
  }
