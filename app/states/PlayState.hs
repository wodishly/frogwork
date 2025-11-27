{- HLINT ignore "Use head" -}
module PlayState where

import Prelude hiding (lookup)

import Allwit
import Blee
import Frog
import Happen
import Key
import Matrix
import Mean
import Random
import Rime
import Shade
import State
import Stavework
import Telly
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
  spitless :: [Mesh],
  spitful :: [Tellywit],
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

makePlayState :: Point -> Meshlist -> PlayState
makePlayState _ Meshlist { bodies, grimes, worldlies } = PlayState {
  seed = formseed,
  spitless = worldlies,
  spitful = [makeTelly (head (tail bodies))],
  frog = makeFrog (head bodies),
  speechframe = makeSpeechframe (head grimes) (concat $ replicate 2 "rɪbɪt "),
  euler = Vertex2 0.3 1.57079633,
  radius = 10,
  programs = [],
  camera = makeCamera,
  writings = []
}

updateFriends :: Allwit -> StateT PlayState IO ()
updateFriends allwit = do
  playwit@PlayState { camera, frog, spitful } <- get
  let viewMatrix = frogLookAt camera.position camera.target
      forward = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)
  newFrog <- lift $ execStateT (updateFrog allwit forward) frog
  newTelly <- lift $ execStateT (updateTelly allwit) (head spitful)
  put playwit { frog = newFrog, spitful = newTelly : tail spitful }

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
  PlayState { frog, spitful, spitless } <- get
  return $
    spitless
    ++ concatMap yoke (frog.meshset : map (.meshset) spitful)
  where yoke Meshset { main, hitframe } = [main, hitframe]

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
