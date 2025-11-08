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

import Key (KeySet, wayward)
import Blee (bg, black)
import Matrix (Point, FrogVector, frogLookAt, frogZero, getProjectionMatrix)
import Random (FrogSeed, defaultSeed)
import Shade (Mesh, drawMesh, setMeshTransform)
import Time (Time, delta)
import Mean (hit)
import Numeric.LinearAlgebra (fromList)
import Rime (cast)
import Numeric.LinearAlgebra.HMatrix
import Control.Monad (when)

data Camera = Camera {
  cPosition :: FrogVector
, cTarget :: FrogVector
} deriving (Show, Eq)

makeCamera :: Camera
makeCamera = Camera {
  cPosition = frogZero
, cTarget = fromList [0, 0, 1]
}

data PlayState = PlayState {
  _seed :: FrogSeed
, _meshes :: [Mesh]
, _lily :: Point
, _euler :: Point
, _programs :: [(Program, VertexArrayObject)]
, _camera :: Camera
}
makeLenses ''PlayState

instance Stately PlayState where
  _name _ = PlayName
  _update = play

instance Show PlayState where
  show (PlayState _ _ l _ p c) = show l ++ show p ++ show c

makePlayState :: [Mesh] -> PlayState
makePlayState ms = PlayState {
  _seed = defaultSeed
, _meshes = ms
, _lily = Vertex2 0 0
, _euler = Vertex2 0.3 1.57079633
, _programs = []
, _camera = makeCamera
}

play :: News -> StateT PlayState IO ()
play (keys, mouse, dis, time) = do
  statewit <- get
  cam <- updateCamera mouse

  let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
  let fwd = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)
  moveFrog keys time fwd

  bg black
  lift $ mapM_ (\m -> drawMesh m (getProjectionMatrix dis) viewMatrix) (statewit^.meshes)

updateCamera :: Point -> StateT PlayState IO Camera
updateCamera (Vertex2 dx dy) = do
  statewit <- get
  let Vertex2 x z = statewit^.lily
  let Vertex2 pitch yaw = statewit^.euler
  let pitch' = pitch + dy / 100.0
  let yaw' = yaw + dx / 100.0
  let fx = 5 * cos yaw * cos pitch
      fy = 5 * sin pitch
      fz = 5 * sin yaw * cos pitch
  let c = Camera {
      cTarget = fromList [x, 0, z]
    , cPosition = fromList [x + fx, 1 + fy, z - fz]
  }
  put statewit { _camera = c, _euler = Vertex2 pitch' yaw' }
  return c

moveFrog :: KeySet -> Time -> Vector GLfloat -> StateT PlayState IO ()
moveFrog keys time fwd = do
  statewit <- get
  let d = Vertex2 (fwd ! 0 ) -(fwd ! 2)
  let Vertex2 _ wy = wayward keys

  let lily' = liftA2 (+)
        ((* (cast (time^.delta) / 1000 * 5)) <$> d)
        (statewit^.lily)
  when (wy < 0) $ put statewit { _lily = lily' }

  updateMesh lily' fwd

updateMesh :: Point -> Vector GLfloat -> StateT PlayState IO ()
updateMesh (Vertex2 x z) fwd = do
  statewit <- get

  let frogPosition = fromList [x, 0, z]
      frogTarget = frogPosition + fromList [fwd!0, 0, fwd!2]
      transform = frogLookAt frogPosition frogTarget
      columns = toColumns transform
      -- awful lol
      c0 = columns !! 0
      c1 = columns !! 1
      c2 = columns !! 2
      transform' = fromColumns [ c0, c1, c2, fromList [x, 0, z, 1] ]
  -- let transform = fromTranslation [x, 0, z]

  newFrog <- lift $ setMeshTransform
    (head $ statewit^.meshes)
    transform'
  put statewit { _meshes = hit 0 (const newFrog) (statewit^.meshes) }