{- HLINT ignore "Use head" -}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module PlayState (
  PlayState (..)
, makePlayState
) where

import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT))
import Numeric.LinearAlgebra (Extractor (..), flatten, fromColumns, fromList, toColumns, (!), (??), (¿))

import Graphics.Rendering.OpenGL as GL (GLfloat, Program, Vertex2 (Vertex2), Vertex3 (Vertex3), VertexArrayObject)

import Frog (Frogwit (position), makeFrog, moveFrog)
import State (News, StateName (..), Stately (..))

import Blee (bg, black)
import Key (arrow)
import Matrix (RenderView (size), frogLookAt, getOrthographicMatrix, getPerspectiveMatrix)
import Mean (given, hit)
import Random (FrogSeed, defaultSeed)
import Rime (Point, Point3, clamp, FrogVector, aught)
import Shade (Mesh (meshAnimation), drawMesh, setMeshTransform)
import Stavemake (Staveware)
import Stavework (stavewrite, Writing, makeWriting)
import Skeleton (play, evermore, once)


data Camera = Camera {
  cPosition :: FrogVector
, cTarget :: FrogVector
} deriving (Show, Eq)

makeCamera :: Camera
makeCamera = Camera {
  cPosition = fromList [0, 0, 0]
, cTarget = fromList [0, 0, 1]
}

data PlayState = PlayState {
  seed :: FrogSeed
, _staveware :: Staveware
, meshes :: [Mesh]
, frog :: Frogwit
, euler :: Point
, radius :: GLfloat
, programs :: [(Program, VertexArrayObject)]
, camera :: Camera
, writings :: [Writing]
}

instance Stately PlayState where
  name _ = PlayName
  staveware = _staveware

  update news = do
    cam <- updateCamera news
    let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
        forward = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)
    _ <- updateFrog news forward
    return ()

  render news@(_, _, display, time) = do
    playwit <- get
    bg black

    -- why does this line make the last written stave have a black background?
    -- renderFeather display time (staveware statewit)

    let cam = camera playwit
    let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
        orthographicMatrix = getOrthographicMatrix display
    lift $ mapM_ (drawMesh (getPerspectiveMatrix display) viewMatrix orthographicMatrix time) (meshes playwit)
    stavewrite news (writings playwit)

instance Show PlayState where
  show (PlayState _ _ _ f _ _ p c _) = show f ++ show p ++ show c

makePlayState :: RenderView -> Staveware -> [Mesh] -> PlayState
makePlayState dis ware ms = PlayState {
  seed = defaultSeed
, _staveware = ware
, meshes = ms
, frog = makeFrog
, euler = Vertex2 0.3 1.57079633
, radius = 5
, programs = []
, camera = makeCamera
, writings = [
    makeWriting "omg frogs!!!!" (Vertex2 (width/2) (height/2))
  ]
} where (width, height) = size dis

updateCamera :: News -> StateT PlayState IO Camera
updateCamera (keys, (pointer, wheel), _, _) = do
  statewit <- get
  let Vertex3 x _ z = position $ frog statewit
  let Vertex2 dx dy = given aught pointer (arrow keys)
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

updateFrog :: News -> FrogVector -> StateT PlayState IO Mesh
updateFrog news forward = do
  statewit <- get
  ((didMove, didJump), newFrog) <- lift $ runStateT (moveFrog news forward) (frog statewit)
  put statewit { frog = newFrog }
  (if didMove then (do
    moveMesh (position $ frog statewit) forward
    animateMesh didMove didJump) else animateMesh didMove didJump)

animateMesh :: Bool -> Bool -> StateT PlayState IO Mesh
animateMesh didMove didJump = do
  statewit <- get
  let frogMesh = head $ meshes statewit
  case meshAnimation frogMesh of
    Just animation -> do
      newAnimation <- lift $ play ((if didJump then once else evermore) animation)
        (if didJump
          then 2
          else if didMove 
            then 0 
            else 5
        )
      let newFrogMesh = frogMesh { meshAnimation = Just newAnimation }
      put statewit { meshes = hit 0 (const newFrogMesh) (meshes statewit) }
      return newFrogMesh
    Nothing -> return frogMesh

moveMesh :: Point3 -> FrogVector -> StateT PlayState IO ()
moveMesh (Vertex3 x y z) forward = do
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
