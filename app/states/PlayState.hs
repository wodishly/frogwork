{- HLINT ignore "Use head" -}
module PlayState (
  PlayState (..)
, makePlayState
, meshes
, programs
, camera
, seed
) where

import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT))
import Numeric.LinearAlgebra (Extractor (..), flatten, fromColumns, fromList, toColumns, (!), (??), (¿))

import Graphics.Rendering.OpenGL as GL (Program, Vertex2 (Vertex2), VertexArrayObject, Vertex3 (Vertex3))

import Frog (Frogwit (..), makeFrog, moveFrog, position)
import FrogState (News, StateName (..), Stately (..))

import Blee (bg, black)
import Matrix (FrogVector, Point, frogLookAt, frogZero, getProjectionMatrix, Point3)
import Mean (hit, ly)
import Random (FrogSeed, defaultSeed)
import Rime (clamp)
import Shade (Mesh, drawMesh, setMeshTransform)
import Key (KeySet, arrow)

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
, _frog :: Frogwit
, _euler :: Point
, _programs :: [(Program, VertexArrayObject)]
, _camera :: Camera
}
makeLenses ''PlayState

instance Stately PlayState where
  _name _ = PlayName
  _update = play

instance Show PlayState where
  show (PlayState _ _ f _ p c) = show f ++ show p ++ show c

makePlayState :: [Mesh] -> PlayState
makePlayState ms = PlayState {
  _seed = defaultSeed
, _meshes = ms
, _frog = makeFrog
, _euler = Vertex2 0.3 1.57079633
, _programs = []
, _camera = makeCamera
}

play :: News -> StateT PlayState IO ()
play news@(keys, _mouse, dis, _time) = do
  statewit <- get
  cam <- updateCamera keys

  let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
  let forward = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)

  updateFrog news forward
  
  bg black
  lift $ mapM_ (drawMesh (getProjectionMatrix dis) viewMatrix) (statewit^.meshes)

updateCamera :: KeySet -> StateT PlayState IO Camera
updateCamera keys = do
  statewit <- get
  let Vertex3 x _ z = statewit^.frog.position
  let Vertex2 dx dy = arrow keys
      Vertex2 pitch yaw = statewit^.euler
      pitch' = clamp (0, 1) $ pitch + dy / 100.0
      yaw' = yaw + dx / 100.0
      fx = 5 * cos yaw * cos pitch
      fy = 5 * sin pitch
      fz = 5 * sin yaw * cos pitch
  let c = Camera {
      cTarget = fromList [x, 0, z]
    , cPosition = fromList [x + fx, 1 + fy, z - fz]
  }
  put statewit { _camera = c, _euler = Vertex2 pitch' yaw' }
  return c

updateFrog :: News -> FrogVector -> StateT PlayState IO ()
updateFrog (keys, _, _, time) forward = do
  statewit <- get
  (didMove, newFrog) <- lift $ runStateT (moveFrog keys time forward) (statewit^.frog)
  put statewit { _frog = newFrog }
  when didMove (updateMesh (statewit^.frog.position) forward)

updateMesh :: Point3 -> FrogVector -> StateT PlayState IO ()
updateMesh (Vertex3 x y z) forward = do
  statewit <- get

  let frogPosition = ly $ fromList [x, y, z]
      frogTarget = frogPosition + fromList [forward!0, 0, forward!2]
      transform = frogLookAt frogPosition frogTarget
      columns = toColumns transform
      -- awful lol
      c0 = columns !! 0
      c1 = columns !! 1
      c2 = columns !! 2
      transform' = fromColumns [ c0, c1, c2, fromList [x, y, z, 1] ]

  newFrogMesh <- lift $ setMeshTransform
    (head $ statewit^.meshes)
    transform'
  put statewit { _meshes = hit 0 (const newFrogMesh) (statewit^.meshes) }
