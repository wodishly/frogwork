{- HLINT ignore "Use head" -}
module PlayState (
  PlayState (..)
, makePlayState
) where

import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, execStateT)

import Numeric.LinearAlgebra (Extractor (..), flatten, fromList, (??), (¿))
import Graphics.Rendering.OpenGL as GL (GLfloat, Program, Vertex2 (Vertex2), Vertex3 (Vertex3), VertexArrayObject)

import Allwit (Allwit (..))
import State (StateName (..), Stately (..))

import Blee (bg, black)
import Frog (Frogwit (position, mesh), makeFrog, updateFrog)
import Happen (Mousewit (..))
import Key (arrow)
import Matrix (RenderView (size), frogLookAt, getOrthographicMatrix, getPerspectiveMatrix)
import Mean (given)
import Random (FrogSeed, defaultSeed)
import Rime (FrogVector, Point, isAught, clamp)
import Shade (Mesh, drawMesh)
import Stavework (makeWriting, stavewrite, Writing)


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

instance Stately PlayState where
  name _ = PlayName

  update allwit = do
    updateCamera allwit
    updateFriends allwit
    return allwit

  render allwit = do
    playwit <- get
    bg black
    drawFriends allwit
    stavewrite allwit (writings playwit)

makePlayState :: RenderView -> [Mesh] -> PlayState
makePlayState dis ms = PlayState {
  seed = defaultSeed
, meshes = tail ms
, frog = makeFrog (head ms)
, euler = Vertex2 0.3 1.57079633
, radius = 5
, programs = []
, camera = makeCamera
, writings = [
    makeWriting "omg frogs!!!!" (Vertex2 (width/2) (height/2))
--  , Writing "Hwæt. We gardena in geardagum, þeodcyninga, þrym gefrunon, hu ða æþelingas ellen fremedon."
--      (Vertex2 0 600) (West, North) (Vertex2 0.3 0.3) red (Say 1 0.05)
--  , Writing "Oft Scyld Scefing sceaþena þreatum, monegum mægþum, meodosetla ofteah, egsode eorlas."
--      (Vertex2 0 576) (West, North) (Vertex2 0.3 0.3) red (Say (1+90*0.05) 0.05)
--  , Writing "Syððan ærest wearð feasceaft funden, he þæs frofre gebad, weox under wolcnum, weorðmyndum þah,"
--      (Vertex2 0 552) (West, North) (Vertex2 0.3 0.3) red (Say (1+(90+85)*0.05) 0.05)
--  , Writing "Oðþæt him æghwylc þara ymbsittendra ofer hronrade hyran scolde, gomban gyldan. þæt wæs god cyning."
--      (Vertex2 0 528) (West, North) (Vertex2 0.3 0.3) red (Say (1+(90+85+98)*0.05) 0.05)
  ]
} where (width, height) = size dis

updateFriends :: Allwit -> StateT PlayState IO ()
updateFriends allwit = do
  playwit <- get
  let cam = camera playwit
      viewMatrix = frogLookAt (cPosition cam) (cTarget cam)
      forward = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)
  newFrog <- lift $ execStateT (updateFrog allwit forward) (frog playwit)
  put playwit { frog = newFrog }

drawFriends :: Allwit -> StateT PlayState IO ()
drawFriends allwit = do
  playwit <- get
  let cam = camera playwit
      viewMatrix = frogLookAt (cPosition cam) (cTarget cam)

  gatherMeshes >>= lift . mapM_ (drawMesh
    (getPerspectiveMatrix $ display allwit)
    viewMatrix
    (getOrthographicMatrix $ display allwit)
    (timewit allwit))

gatherMeshes :: StateT PlayState IO [Mesh]
gatherMeshes = do
  playwit <- get
  return $ meshes playwit ++ [mesh $ frog playwit]

updateCamera :: Allwit -> StateT PlayState IO ()
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

  put playwit {
    camera = c
  , euler = Vertex2 pitch' yaw'
  , radius = r
  }
