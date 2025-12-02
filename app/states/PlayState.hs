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
import Rime
import Shade
import State
import Stavework
import Telly
import Weird


data Camera = Camera {
  position :: FrogVector,
  target :: FrogVector
} deriving (Show, Eq)

makeCamera :: Camera
makeCamera = Camera
  (fromList [0, 0, 0])
  (fromList [0, 0, 1])

data PlayState = PlayState {
  spitless :: [Mesh],
  spitful :: [Tellywit],
  frog :: Frogwit,
  speechframe :: Speechframe,
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
    bg black
    drawFriends allwit
    stave writings allwit

    -- doAtEach 1 (timewit allwit) (lift fremdcroak)
    -- lift $ when (weird < (1/60)) fremdcroak

    return allwit

  stave lens allwit = do
    playwit <- get

    when (fromMaybe False $ lookup ShowSpeech allwit.settings) $ do
      ws <- stavewriteAll allwit (view lens playwit)
      drawSpeechframe allwit
      put $ set lens ws playwit

makePlayState :: Allwit -> Point -> Meshlist -> (Allwit, PlayState)
makePlayState allwit _ Meshlist { bodies, grimes = (stavegrime, _), worldlies } = (allwit', PlayState {
  spitless = worldlies,
  spitful = [makeTelly (Vertex3 (ws!!0) 0 (ws!!1)) (Vertex3 -1 0 -1, Vertex3 1 2 1) (head (tail bodies))],
  frog = makeFrog (head bodies),
  speechframe = makeSpeechframe stavegrime (concat $ replicate 2 "rɪbɪt "),
  euler = Vertex2 0.3 1.57079633,
  radius = 10,
  programs = [],
  camera = makeCamera,
  _writings = []
}) where (ws, allwit') = first (map ((*4) . (- 0.5)) . ly) (weirds 2 allwit)

updateFriends :: Allwit -> StateT PlayState IO ()
updateFriends allwit = do
  playwit@PlayState { camera, frog, spitful } <- get
  let viewMatrix = frogLookAt camera.position camera.target
      forward = flatten $ (viewMatrix ¿ [2]) ?? (Take 3, All)
  newFrog <- lift $ execStateT (updateFrog allwit forward) frog
  newTelly <- lift $ execStateT (updateTelly allwit) (head spitful)
  put playwit { frog = newFrog, spitful = newTelly : tail spitful }

drawFriends :: Allwit -> StateT PlayState IO ()
drawFriends allwit = do
  PlayState { camera } <- get

  gatherMeshes >>= lift . mapM_ (drawWith allwit (frogLookAt camera.position camera.target))

  where
  gatherMeshes :: StateT PlayState IO [Mesh]
  gatherMeshes = do
    PlayState { frog, spitful, spitless } <- get

    return $ spitless ++ concatMap yoke (frog.meshset : map (.meshset) spitful)
      where yoke Meshset { main, hitframe } = [main, hitframe]

drawSpeechframe :: Allwit -> StateT PlayState IO ()
drawSpeechframe allwit@(Allwit { staveware = (book, _) }) = do
  playwit@PlayState {
    camera,
    speechframe = _speechframe@Speechframe { mesh, writtens }
  } <- get

  lift $ drawWith allwit (frogLookAt camera.position camera.target) mesh

  ws <- stavewriteAll allwit (fromMaybe (flayLines _speechframe book) writtens)
  put playwit { speechframe = _speechframe { writtens = Just ws } }

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
