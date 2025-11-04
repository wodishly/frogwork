{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PlayState where

import Control.Lens
import Control.Monad.State

import Foreign (new)
import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified Graphics.Rendering.OpenGL as GL (get)
import qualified Data.HashMap.Strict as HM
import SDL (windowSize, V2 (V2))
import Rime (cast)

import FrogState
import Light
import Key
import Shade
import Time
import Mean
import Random

data PlayState = MkPlayState {
  _seed :: FrogSeed,
  _meshes :: [Mesh],
  _lily :: Point,
  _programs :: [(Program, VertexArrayObject)]
}
makeLenses ''PlayState

type PlayUpdate = News -> StateT PlayState IO ()

instance Stately PlayState where
  _name _ = Play

makePlayState :: PlayState
makePlayState = MkPlayState {
  _seed = defaultSeed,
  _meshes = [],
  _lily = Vertex2 0 0,
  _programs = []
}

playState :: News -> StateT PlayState IO ()
playState (_, keys, window, time) = do
  statewit <- get

  move keys time
  lift $ bg green

  V2 windowWidth windowHeight <- (cast <$>) <$> GL.get (windowSize window)
  viewport $= (Position 0 0, Size windowWidth windowHeight)

  let projectionMatrix = getProjectionMatrix RenderView {
    aspect = fromIntegral windowWidth / fromIntegral windowHeight,
    fov = pi / 4.0,
    near = 0.1,
    far = 100.0
  }

  -- mesh rendering --
  updatePlayerUniforms
  lift $ mapM_ (`drawMesh` projectionMatrix) (statewit^.meshes)

updatePlayerUniforms :: StateT PlayState IO ()
updatePlayerUniforms = do
  statewit <- get
  -- assume player = first mesh
  let player = head (statewit^.meshes)
  lift $ useMesh player
  let uniforms = uniformMap player

  -- is `new` appropriate here? 
  lilyPtr <- lift $ new (statewit^.lily)
  inputLocation <- lift $ uniforms HM.! "u_input2d"
  lift $ uniformv inputLocation 1 lilyPtr
  return ()

move :: KeySet -> Time -> StateT PlayState IO ()
move keys time = do
  statewit <- get
  put $ statewit {
    _lily = liftA2 (+)
      ((* (200 * throttle time)) <$> wayward keys)
      (statewit^.lily)
  }
