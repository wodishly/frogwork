{- HLINT ignore "Use head" -}
module PlayState (
  PlayState (..),
    speechframe,
  makePlayState,
  writings
) where

import Prelude hiding (lookup)

import Control.Lens (makeLenses)
import Control.Monad (when, void)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT, execStateT)
import Data.Map (lookup)
import Data.Maybe (fromMaybe)

import Numeric.LinearAlgebra (Extractor (..), flatten, fromList, (??), (¿))
import Graphics.Rendering.OpenGL as GL (GLfloat, Program, Vertex2 (Vertex2), Vertex3 (Vertex3), VertexArrayObject)

import Allwit (Allwit (..), UnholyMeshMash, Setting (ShowSpeech))
import State (StateName (..), Stately (..))

import Blee (bg, black)
import Frog (makeFrog, updateFrog, Frogwit (Frogwit), mesh, position, fresh)
import Happen (Mousewit (..))
import Key (arrow)
import Matrix (frogLookAt, getOrthographicMatrix, getPerspectiveMatrix)
import Mean (given)
import Random (FrogSeed, defaultSeed)
import Rime (FrogVector, Point, clamp, isAught)
import Shade (Mesh, drawMesh)
import Stavework (Speechframe (meesh), Writing, makeSpeechframe, makeWriting, speechwrite, stavewriteAll)

data Camera = Camera {
  cPosition :: FrogVector,
  cTarget :: FrogVector
} deriving (Show, Eq)

makeCamera :: Camera
makeCamera = Camera
  (fromList [0, 0, 0])
  (fromList [0, 0, 1])

data PlayState = PlayState {
  seed :: FrogSeed,
  meshes :: [Mesh],
  frog :: Frogwit,
  _speechframe :: Speechframe,
  euler :: Point,
  radius :: GLfloat,
  programs :: [(Program, VertexArrayObject)],
  camera :: Camera,
  _writings :: [Writing]
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
    ws <- stavewriteAll allwit (_writings playwit)
    put playwit { _writings = ws }
    drawSpeech allwit

makePlayState :: Point -> UnholyMeshMash -> PlayState
makePlayState (Vertex2 w0 h0) (f, ff, sp, rest) = PlayState {
  seed = defaultSeed,
  meshes = rest,
  frog = makeFrog f ff,
  -- _speechframe = makeSpeechframe sp "rɪbɪt rɪbɪt rɪbɪt rɪbɪt rɪbɪt rɪbɪt rɪbɪt rɪbɪt rɪbɪt rɪbɪt rɪbɪt rɪbɪt rɪbɪt rɪbɪt ",--A frog is any member of a diverse and largely semiaquatic group of short-bodied, tailless amphibian vertebrates composing the order Anura (coming from the Ancient Greek ανουρα, literally 'without tail')."
  _speechframe = makeSpeechframe sp "A frog is any member of a diverse and largely semiaquatic group of short-bodied, tailless amphibian vertebrates composing the order Anura (coming from the Ancient Greek ανουρα, literally 'without tail').",
  -- _speechframe = makeSpeechframe sp (concat $ replicate 128 "i "),
  euler = Vertex2 0.3 1.57079633,
  radius = 10,
  programs = [],
  camera = makeCamera,
  _writings = [
    -- makeWriting (Vertex2 (w0/2) (h0/2)) "omg frogs!!!!"
--  , Writing "Hwæt. We gardena in geardagum, þeodcyninga, þrym gefrunon, hu ða æþelingas ellen fremedon."
--      (Vertex2 0 600) (West, North) (Vertex2 0.3 0.3) red (Say 1 0.05)
--  , Writing "Oft Scyld Scefing sceaþena þreatum, monegum mægþum, meodosetla ofteah, egsode eorlas."
--      (Vertex2 0 576) (West, North) (Vertex2 0.3 0.3) red (Say (1+90*0.05) 0.05)
--  , Writing "Syððan ærest wearð feasceaft funden, he þæs frofre gebad, weox under wolcnum, weorðmyndum þah,"
--      (Vertex2 0 552) (West, North) (Vertex2 0.3 0.3) red (Say (1+(90+85)*0.05) 0.05)
--  , Writing "Oðþæt him æghwylc þara ymbsittendra ofer hronrade hyran scolde, gomban gyldan. þæt wæs god cyning."
--      (Vertex2 0 528) (West, North) (Vertex2 0.3 0.3) red (Say (1+(90+85+98)*0.05) 0.05)
  ]
}

updateFriends :: Allwit -> StateT PlayState IO ()
updateFriends allwit = do
  playwit@PlayState { camera, frog } <- get
  let viewMatrix = frogLookAt (cPosition camera) (cTarget camera)
      forward = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)
  newFrog <- lift $ execStateT (updateFrog allwit forward) frog
  put playwit { frog = newFrog }

drawFriends :: Allwit -> StateT PlayState IO ()
drawFriends (Allwit { display, timewit }) = do
  PlayState { camera } <- get

  gatherMeshes >>= lift . mapM_ (drawMesh
    (getPerspectiveMatrix display)
    (frogLookAt (cPosition camera) (cTarget camera))
    (getOrthographicMatrix display)
    timewit)

gatherMeshes :: StateT PlayState IO [Mesh]
gatherMeshes = do
  PlayState { meshes, frog = Frogwit { mesh, fresh } } <- get
  return $ meshes ++ [mesh, fresh]

drawSpeech :: Allwit -> StateT PlayState IO ()
drawSpeech allwit@(Allwit { settings, display, timewit }) = do
  playwit@(PlayState { camera, _speechframe }) <- get
  when (fromMaybe False (lookup ShowSpeech settings)) $ do
    lift $ drawMesh
      (getPerspectiveMatrix display)
      (frogLookAt (cPosition camera) (cTarget camera))
      (getOrthographicMatrix display)
      timewit
      (meesh _speechframe)
    x <- speechwrite allwit _speechframe
    put playwit { _speechframe = x }

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
      cTarget = fromList [x, y, z],
      cPosition = fromList [x + fx, y + fy + 1, z - fz]
    },
    euler = Vertex2 pitch' yaw',
    radius = r
  }
