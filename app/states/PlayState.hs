{- HLINT ignore "Use head" -}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module PlayState (
  PlayState (..)
, makePlayState
) where

import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT))
import Data.Maybe (fromJust, isJust)

import Numeric.LinearAlgebra (Extractor (..), flatten, fromColumns, fromList, toColumns, (!), (??), (¿))
import Graphics.Rendering.OpenGL as GL (GLfloat, Program, Vertex2 (Vertex2), Vertex3 (Vertex3), VertexArrayObject)

import Allwit (Allwit (..))
import State (StateName (..), Stately (..))

import Blee (bg, black)
import Frog (Frogwit (position), makeFrog, moveFrog)
import Happen (Mousewit (..))
import Key (arrow)
import Matrix (RenderView (size), frogLookAt, getOrthographicMatrix, getPerspectiveMatrix)
import Mean (given, hit)
import Random (FrogSeed, defaultSeed)
import Rime (FrogVector, Point, Point3, isAught, clamp)
import Shade (Mesh (meshAnimation), drawMesh, setMeshTransform)
import Skeleton (evermore, once, play)
import Stavework (Writing, makeWriting, stavewrite)
import Time (Timewit (lifetime))


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
, meshes :: [Mesh]
, frog :: Frogwit
, euler :: Point
, radius :: GLfloat
, programs :: [(Program, VertexArrayObject)]
, camera :: Camera
, writings :: [Writing]
}

instance Show PlayState where
  show (PlayState _ _ f _ _ p c _) = show f ++ show p ++ show c

instance Stately PlayState where
  name _ = PlayName

  update allwit = do
    cam <- updateCamera allwit
    let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
        forward = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)
    updateFrog allwit forward
    return allwit

  render allwit = do
    playwit <- get
    bg black

    -- why does this line make the last written stave have a black background?
    -- renderFeather display time (staveware statewit)

    let cam = camera playwit
    let viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
        
    lift $ mapM_ (drawMesh
        (getPerspectiveMatrix $ display allwit)
        viewMatrix
        (getOrthographicMatrix $ display allwit)
        (timewit allwit))
      (meshes playwit)
    stavewrite allwit (writings playwit)

makePlayState :: RenderView -> [Mesh] -> PlayState
makePlayState dis ms = PlayState {
  seed = defaultSeed
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

updateCamera :: Allwit -> StateT PlayState IO Camera
updateCamera allwit = do
  playwit <- get
  let Vertex3 x _ z = position $ frog playwit
      Vertex2 dx dy = given isAught (pointer $ mouse allwit) (arrow $ keyset allwit)
      Vertex2 pitch yaw = euler playwit
      pitch' = clamp (0, 1) $ pitch + dy / 100.0
      yaw' = yaw + dx / 100.0
      Vertex2 _ wy = wheel $ mouse allwit
      r = clamp (3, 25) $ radius playwit - wy
      fx = r * cos yaw * cos pitch
      fy = r * sin pitch
      fz = r * sin yaw * cos pitch
      c = Camera {
        cTarget = fromList [x, 0, z]
      , cPosition = fromList [x + fx, 1 + fy, z - fz]
      }
  put playwit { camera = c, euler = Vertex2 pitch' yaw', radius = r }
  return c

updateFrog :: Allwit -> FrogVector -> StateT PlayState IO ()
updateFrog allwit forward = do
  playwit <- get
  ((didMove, didLeap), newFrog) <- lift $ runStateT (moveFrog allwit forward) (frog playwit)

  put playwit { frog = newFrog }

  when didMove (moveMesh forward $ position newFrog)
  animateMesh didMove didLeap (lifetime $ timewit allwit)

animateMesh :: Bool -> Bool -> Float -> StateT PlayState IO ()
animateMesh didMove didLeap now = do
  playwit <- get
  let frogMesh = head $ meshes playwit
      athem = meshAnimation frogMesh
  when (isJust athem) $
    let newAnimation = play now
          ((if didLeap then once else evermore) (fromJust athem))
          (if didLeap then 2 else if didMove then 0 else 5)
        newFrogMesh = frogMesh { meshAnimation = Just newAnimation }
    in put playwit { meshes = hit 0 (const newFrogMesh) (meshes playwit) }

moveMesh :: FrogVector -> Point3 -> StateT PlayState IO ()
moveMesh forward (Vertex3 x y z) = do
  playwit <- get

  let frogPosition = fromList [x, y, z]
      frogTarget = frogPosition + fromList [forward!0, 0, forward!2]
      transform = frogLookAt frogPosition frogTarget
      columns = toColumns transform
      -- awesome lol
      transform' = fromColumns [
          columns !! 0
        , columns !! 1
        , columns !! 2
        , fromList [x, y, z, 1]
        ]

  newFrogMesh <- lift $ setMeshTransform transform' (head $ meshes playwit)
  put playwit { meshes = hit 0 (const newFrogMesh) (meshes playwit) }
