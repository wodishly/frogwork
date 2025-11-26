{- HLINT ignore "Use head" -}
module PlayState where

import Prelude hiding (lookup)

import Control.Lens (makeLenses)
import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, execStateT)
import Data.Map (lookup)
import Data.Maybe

import Numeric.LinearAlgebra (Extractor (..), flatten, fromList, (??), (¿))
import Graphics.Rendering.OpenGL as GL (GLfloat, Program, Vertex2 (Vertex2), Vertex3 (Vertex3), VertexArrayObject)

import Allwit
import State

import Blee
import Frog
import Happen
import Key
import Matrix
import Mean
import Random
import Rime
import Shade
import Stavework
import Tung


data Camera = Camera {
  position :: FrogVector,
  target :: FrogVector
} deriving (Show, Eq)

makeCamera :: Camera
makeCamera = Camera
  (fromList [0, 0, 0])
  (fromList [0, 0, 1])

data PlayState = PlayState {
  seed :: FrogSeed,
  meshes :: [Mesh],
  frog :: Frogwit,
  speechframe :: Speechframe,
  euler :: Point,
  radius :: GLfloat,
  programs :: [(Program, VertexArrayObject)],
  camera :: Camera,
  writings :: [Writing]
}
makeLenses ''PlayState

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
    ws <- stavewriteAll allwit (writings playwit)
    put playwit { writings = ws }
    drawSpeech allwit
    doAtEach 1 (timewit allwit) (lift fremdcroak)
    lift $ maybe (return ()) (const fremdcroak) =<< rMaybeLoud (1/60)
    return allwit

makePlayState :: Point -> UnholyMeshMash -> PlayState
makePlayState _ (f, ff, sp, rest) = PlayState {
  seed = formseed,
  meshes = rest,
  frog = makeFrog f ff,
  speechframe = makeSpeechframe sp (concat $ replicate 2 "rɪbɪt "),
  euler = Vertex2 0.3 1.57079633,
  radius = 10,
  programs = [],
  camera = makeCamera,
  writings = []
}

updateFriends :: Allwit -> StateT PlayState IO ()
updateFriends allwit = do
  playwit@PlayState { camera, frog } <- get
  let viewMatrix = frogLookAt camera.position camera.target
      forward = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)
  newFrog <- lift $ execStateT (updateFrog allwit forward) frog
  put playwit { frog = newFrog }

drawFriends :: Allwit -> StateT PlayState IO ()
drawFriends Allwit { display, timewit } = do
  PlayState { camera } <- get

  gatherMeshes >>= lift . mapM_ (drawMesh
    (getPerspectiveMatrix display)
    (frogLookAt camera.position camera.target)
    (getOrthographicMatrix display)
    timewit)

gatherMeshes :: StateT PlayState IO [Mesh]
gatherMeshes = do
  PlayState { meshes, frog = Frogwit { mesh, fresh } } <- get
  return $ meshes ++ [mesh, fresh]

drawSpeech :: Allwit -> StateT PlayState IO ()
drawSpeech allwit@(Allwit { settings, display, timewit }) = do
  playwit@(PlayState { camera, speechframe = _speechframe@Speechframe { meesh } }) <- get
  when (fromMaybe False (lookup ShowSpeech settings)) $ do
    lift $ drawMesh
      (getPerspectiveMatrix display)
      (frogLookAt camera.position camera.target)
      (getOrthographicMatrix display)
      timewit
      meesh
    x <- speechwrite allwit _speechframe
    put playwit { speechframe = x }

updateCamera :: Allwit -> StateT PlayState IO ()
updateCamera Allwit {
  keyset,
  mouse = Mousewit {
    pointer,
    wheel = Vertex2 _ wy
  }
} = do
  playwit@PlayState {
    frog = Frogwit { position = Vertex3 x y z },
    euler = Vertex2 pitch yaw,
    radius
  } <- get

  let Vertex2 dx dy = given isAught pointer (arrow keyset)
      pitch' = clamp (0, 1) $ pitch + dy / 100.0
      yaw' = yaw + dx / 100.0
      r = clamp (3, 25) $ radius - wy
      fx = r * cos yaw * cos pitch
      fy = r * sin pitch
      fz = r * sin yaw * cos pitch

  put playwit {
    camera = Camera {
      target = fromList [x, y, z],
      position = fromList [x + fx, y + fy + 1, z - fz]
    },
    euler = Vertex2 pitch' yaw',
    radius = r
  }
