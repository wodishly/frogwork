{- HLINT ignore "Use head" -}
module PlayState (
  PlayState (..)
, makePlayState
, meshes
, programs
, camera
, seed
, staveware -- unused
) where

import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT))
import Numeric.LinearAlgebra (Extractor (..), flatten, fromColumns, fromList, toColumns, (!), (??), (¿))

import Graphics.Rendering.OpenGL as GL (Program, Vertex2 (Vertex2), VertexArrayObject, Vertex3 (Vertex3), GLfloat)

import Frog (Frogwit (..), makeFrog, moveFrog, position)
import State (News, StateName (..), Stately (..))

import Blee (bg, black)
import Key (arrow)
import Matrix (FrogVector, Point, Point3, aught, frogLookAt, frogZero, getPerspectiveMatrix, getOrthographicMatrix)
import Mean (given, hit)
import Random (FrogSeed, defaultSeed)
import Rime (clamp)
import Shade (Mesh, drawMesh, setMeshTransform)
import Stave (Staveware, stavewrite)

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
, _staveware :: Staveware
, _meshes :: [Mesh]
, _frog :: Frogwit
, _euler :: Point
, _radius :: GLfloat
, _programs :: [(Program, VertexArrayObject)]
, _camera :: Camera
}
makeLenses ''PlayState

instance Stately PlayState where
  name _ = PlayName
  update news = do
    cam <- updateCamera news
    let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
        forward = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)
    updateFrog news forward

  render (_, _, _, display, _) = do
    statewit <- get
    bg black
    let cam = statewit^.camera
    let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
        orthographicMatrix = getOrthographicMatrix display
    lift $ mapM_ (drawMesh (getPerspectiveMatrix display) viewMatrix orthographicMatrix) (statewit^.meshes)
    lift $ stavewrite (statewit^.staveware) (Vertex2 0 0) 1 "FROG"

instance Show PlayState where
  show (PlayState _ _ _ f _ _ p c) = show f ++ show p ++ show c

makePlayState :: Staveware -> [Mesh] -> PlayState
makePlayState ware ms = PlayState {
  _seed = defaultSeed
, _staveware = ware
, _meshes = ms
, _frog = makeFrog
, _euler = Vertex2 0.3 1.57079633
, _radius = 5
, _programs = []
, _camera = makeCamera
}

updateCamera :: News -> StateT PlayState IO Camera
updateCamera (keys, mouse, wheel, _, _) = do
  statewit <- get
  let Vertex3 x _ z = statewit^.frog.position
  let Vertex2 dx dy = given aught mouse (arrow keys)
      Vertex2 pitch yaw = statewit^.euler
      pitch' = clamp (0, 1) $ pitch + dy / 100.0
      yaw' = yaw + dx / 100.0
      Vertex2 _ wy = wheel
      r = clamp (3, 25) $ statewit^.radius - wy
      fx = r * cos yaw * cos pitch
      fy = r * sin pitch
      fz = r * sin yaw * cos pitch
  let c = Camera {
      cTarget = fromList [x, 0, z]
    , cPosition = fromList [x + fx, 1 + fy, z - fz]
  }
  put statewit { _camera = c, _euler = Vertex2 pitch' yaw', _radius = r }
  return c

updateFrog :: News -> FrogVector -> StateT PlayState IO ()
updateFrog news forward = do
  statewit <- get
  (didMove, newFrog) <- lift $ runStateT (moveFrog news forward) (statewit^.frog)
  put statewit { _frog = newFrog }
  when didMove (updateMesh (statewit^.frog.position) forward)

updateMesh :: Point3 -> FrogVector -> StateT PlayState IO ()
updateMesh (Vertex3 x y z) forward = do
  statewit <- get

  let frogPosition = fromList [x, y, z]
      frogTarget = frogPosition + fromList [forward!0, 0, forward!2]
      transform = frogLookAt frogPosition frogTarget
      columns = toColumns transform
      -- awesome lol
      c0 = columns !! 0
      c1 = columns !! 1
      c2 = columns !! 2
      transform' = fromColumns [ c0, c1, c2, fromList [x, y, z, 1] ]

  newFrogMesh <- lift $ setMeshTransform transform' (head $ statewit^.meshes)
  put statewit { _meshes = hit 0 (const newFrogMesh) (statewit^.meshes) }
