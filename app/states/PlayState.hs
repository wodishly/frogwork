module PlayState (
  PlayState (..)
, makePlayState
, meshes
, programs
, camera
, seed
) where

import Control.Lens (makeLenses, (^.))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT)

import Graphics.Rendering.OpenGL as GL hiding (get)

import FrogState (News, Stately (..), StateName (..))

import Key (KeySet, wayward, keyBegun)
import Blee (bg, black)
import Matrix (Point, FrogVector, frogLookAt, frogZero, getProjectionMatrix, fromTranslation)
import Random (FrogSeed, defaultSeed)
import Shade (Mesh, drawMesh, setMeshTransform)
import Time (Time, delta)
import Mean (hit)
import Numeric.LinearAlgebra (fromList)
import Control.Monad (when)
import SDL.Input.Keyboard.Codes

data Camera = Camera {
  cPosition :: FrogVector
, cTarget :: FrogVector
} deriving (Show, Eq)

data Frog = Frog {
    s :: Point
  , v :: Point
} deriving (Show, Eq)

earthWeight :: Point
earthWeight = Vertex2 0 1

makeCamera :: Camera
makeCamera = Camera {
  cPosition = frogZero
, cTarget = fromList [0, 0, 1]
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
  _name _ = PlayName
  _update = play

instance Show PlayState where
  show (PlayState _ _ l p c) = show l ++ show p ++ show c

makePlayState :: [Mesh] -> PlayState
makePlayState ms = PlayState {
  _seed = defaultSeed
, _meshes = ms
, _lily = Vertex2 0 0
, _programs = []
, _camera = makeCamera
}

play :: News -> StateT PlayState IO ()
play (keys, mouse, dis, time) = do
  statewit <- get
  cam <- updateCamera mouse

  leap keys
  moveFrog keys time

  let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)

  bg black
  lift $ mapM_ (\m -> drawMesh m (getProjectionMatrix dis) viewMatrix) (statewit^.meshes)

updateCamera :: Point -> StateT PlayState IO Camera
updateCamera (Vertex2 _x _y) = do
  statewit <- get
  let Vertex2 x z = statewit^.lily
  let c = Camera {
      cTarget = fromList [x, 1, z]
    , cPosition = fromList [x, 3, z - 5]
  }
  put statewit { _camera = c }
  return c

leap :: KeySet -> StateT PlayState IO ()
leap keys = do
  statewit <- get
  when (keyBegun keys ScancodeSpace)
    (put statewit { _lily = liftA2 (+) (statewit^.lily) (Vertex2 0 0.1) })

moveFrog :: KeySet -> Time -> StateT PlayState IO ()
moveFrog keys time = do
  statewit <- get
  let lily' = liftA2 (+)
        ((* (fromIntegral (time^.delta)/1000)) <$> wayward keys)
        (statewit^.lily)

  put statewit { _lily = lily' }
  updateMesh lily'

updateMesh :: Point -> StateT PlayState IO ()
updateMesh (Vertex2 x z) = do
  statewit <- get
  newFrog <- lift $ setMeshTransform
    (head $ statewit^.meshes)
    (fromTranslation [x, 1, z])
  put statewit { _meshes = hit 0 (const newFrog) (statewit^.meshes) }