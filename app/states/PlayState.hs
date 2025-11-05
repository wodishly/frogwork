{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PlayState where

import Control.Lens
import Control.Monad.State

import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified Graphics.Rendering.OpenGL as GL (get)
import SDL (windowSize)
import SDL.Vect hiding (Point)
import Rime (cast)

import FrogState
import Light
import Key
import Shade
import Time
import Random
import Matrix
import Mean

data Camera = Camera {
    cPosition :: FrogVector
  , cTarget :: FrogVector
} deriving (Show, Eq)

makeCamera :: Camera
makeCamera = Camera {
    cPosition = frogZero
  , cTarget = [0, 0, 1]
}

data PlayState = PlayState {
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

instance Show PlayState where
  show (PlayState _ _ l p c) = show l ++ show p ++ show c

makePlayState :: PlayState
makePlayState = PlayState {
    _seed = defaultSeed
  , _meshes = []
  , _lily = Vertex2 0 0
  , _programs = []
  , _camera = makeCamera
}

playState :: News -> StateT PlayState IO ()
playState (keys, window, time) = do
  statewit <- get

  move keys time
  lift $ bg black

  V2 windowWidth windowHeight <- (cast <$>) <$> GL.get (windowSize window)
  viewport $= (Position 0 0, Size windowWidth windowHeight)
  let display = RenderView {
    _aspect = fromIntegral windowWidth / fromIntegral windowHeight,
    _fov = pi / 4.0,
    _near = 0.1,
    _far = 100.0
  }
  let projectionMatrix = getProjectionMatrix display

  let Vertex2 a b = statewit^.lily
  let c = Camera {
    cTarget=[cos (pi/2+a/10000),-1,sin (pi/2+a/10000)-1+b/10000],
    cPosition=[0,-1,-1+b/10000]
  }
  let viewMatrix = frogLookAt (cPosition c) (cTarget c)
  -- print viewMatrix
  -- let lapl = detLaplace $ unhew viewMatrix
  -- print lapl

  -- mesh rendering --
  let m = statewit^.meshes
  lift $ mapM_ (\mesh -> drawMesh mesh projectionMatrix viewMatrix) m

  updateCamera c

updateCamera :: Camera -> StateT PlayState IO ()
updateCamera c = do
  statewit <- get
  put $ statewit {
    _camera = c
  }

move :: KeySet -> Time -> StateT PlayState IO ()
move keys time = do
  statewit <- get
  put $ statewit {
    _lily = liftA2 (+)
        ((* (200 * throttle time)) <$> wayward keys)
        (statewit^.lily)
  }
