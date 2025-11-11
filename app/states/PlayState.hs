{- HLINT ignore "Use head" -}
module PlayState (
  PlayState (..)
, makePlayState
) where

import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT))
import Numeric.LinearAlgebra (Extractor (..), flatten, fromColumns, fromList, toColumns, (!), (??), (¿))

import Graphics.Rendering.OpenGL as GL (Program, Vertex2 (Vertex2), VertexArrayObject, Vertex3 (Vertex3), GLfloat)

import Frog (Frogwit (position), makeFrog, moveFrog)
import State (News, StateName (..), Stately (..))

import Blee (bg, black)
import Key (arrow)
import Matrix (FrogVector, RenderView (size), frogLookAt, frogZero, getOrthographicMatrix, getPerspectiveMatrix)
import Mean (given, hit)
import Random (FrogSeed, defaultSeed)
import Rime (Point, Point3, aught, clamp)
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
  seed :: FrogSeed
, staveware :: Staveware
, meshes :: [Mesh]
, frog :: Frogwit
, euler :: Point
, radius :: GLfloat
, programs :: [(Program, VertexArrayObject)]
, camera :: Camera
}

instance Stately PlayState where
  name _ = PlayName
  update news = do
    cam <- updateCamera news
    let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
        forward = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)
    updateFrog news forward

  render (_, _, _, display, time) = do
    statewit <- get
    bg black
    let cam = camera statewit
    let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
        orthographicMatrix = getOrthographicMatrix display
        (width, height) = size display
    lift $ mapM_ (drawMesh (getPerspectiveMatrix display) viewMatrix orthographicMatrix time) (meshes statewit)
    lift $ stavewrite (staveware statewit) (Vertex2 (0.1*width) $ 0.9*height) 1 "omg frogs!!!!"

instance Show PlayState where
  show (PlayState _ _ _ f _ _ p c) = show f ++ show p ++ show c

makePlayState :: Staveware -> [Mesh] -> PlayState
makePlayState ware ms = PlayState {
  seed = defaultSeed
, staveware = ware
, meshes = ms
, frog = makeFrog
, euler = Vertex2 0.3 1.57079633
, radius = 5
, programs = []
, camera = makeCamera
}

updateCamera :: News -> StateT PlayState IO Camera
updateCamera (keys, mouse, wheel, _, _) = do
  statewit <- get
  let Vertex3 x _ z = position $ frog statewit
  let Vertex2 dx dy = given aught mouse (arrow keys)
      Vertex2 pitch yaw = euler statewit
      pitch' = clamp (0, 1) $ pitch + dy / 100.0
      yaw' = yaw + dx / 100.0
      Vertex2 _ wy = wheel
      r = clamp (3, 25) $ radius statewit - wy
      fx = r * cos yaw * cos pitch
      fy = r * sin pitch
      fz = r * sin yaw * cos pitch
  let c = Camera {
      cTarget = fromList [x, 0, z]
    , cPosition = fromList [x + fx, 1 + fy, z - fz]
  }
  put statewit { camera = c, euler = Vertex2 pitch' yaw', radius = r }
  return c

updateFrog :: News -> FrogVector -> StateT PlayState IO ()
updateFrog news forward = do
  statewit <- get
  (didMove, newFrog) <- lift $ runStateT (moveFrog news forward) (frog statewit)
  put statewit { frog = newFrog }
  when didMove (updateMesh (position $ frog statewit) forward)

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

  newFrogMesh <- lift $ setMeshTransform transform' (head $ meshes statewit)
  put statewit { meshes = hit 0 (const newFrogMesh) (meshes statewit) }
