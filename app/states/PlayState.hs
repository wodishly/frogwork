module PlayState (
  PlayState (..)
, makePlayState
, meshes
, programs
, camera
, seed
) where

import Control.Lens (makeLenses, (^.))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT)

import Graphics.Rendering.OpenGL as GL hiding (get)

import FrogState (News, Stately (..), StateName (..), preent)

import Key (KeySet, wayward)
import Light (Point, bg, black)
import Matrix (FrogVector, frogLookAt, frogZero, getProjectionMatrix, fromTranslation)
import Random (FrogSeed, defaultSeed)
import Shade (Mesh, drawMesh, setMeshTransform)
import Time (Time, throttle, deltaTime, deltas, lifetime)
import Mean (hit, ly)
import Rime (cast)

data Camera = Camera {
    cPosition :: FrogVector
  , cTarget :: FrogVector
} deriving (Show, Eq)

makeCamera :: Camera
makeCamera = Camera {
    cPosition = frogZero
  , cTarget = [0, 0, 1]
}

data PlayState = PlayState {
    _seed :: FrogSeed
  , _meshes :: [Mesh]
  , _lily :: Point
  , _programs :: [(Program, VertexArrayObject)]
  , _camera :: Camera
}
makeLenses ''PlayState

instance Stately PlayState where
  _name _ = PlayName
  _update = play

instance Show PlayState where
  show (PlayState _ _ l p c) = show l ++ show p ++ show c

makePlayState :: PlayState
makePlayState = PlayState {
    _seed = defaultSeed
  , _meshes = []
  , _lily = Vertex2 0 0
  , _programs = []
  , _camera = makeCamera
}

play :: News -> StateT PlayState IO ()
play (keys, mouse, dis, time) = do
  statewit <- get

  move keys time
  bg black



  let projectionMatrix = getProjectionMatrix dis

  -- let Vertex2 a b = statewit^.lily
  let Vertex2 a b = mouse
  let offset = 1
  let c = Camera {
      cTarget = (\(Vertex2 x z) -> [
        x
      , -2
      , z
      ]) $ statewit^.lily
      --   [
      --   cos (pi/2 + a/10000)
      -- , -1
      -- , -(sin (pi/2 + a/10000) - 1 + b/10000)
      -- ]
    , cPosition = (\(Vertex2 x z) -> [
        x + offset * cos (cast (time^.lifetime) / 1000)
      , -2
      , z - offset * sin (cast (time^.lifetime) / 1000)
      ]) $ statewit^.lily-- $ [0, -1, -1 + b/10000]
  }
  let viewMatrix = frogLookAt (cPosition c) (cTarget c)
  -- print viewMatrix
  -- let lapl = detLaplace $ unhew viewMatrix
  -- print lapl

  -- mesh rendering --
  let m = statewit^.meshes
  lift $ mapM_ (\mesh -> drawMesh mesh projectionMatrix viewMatrix) m

  updateCamera c

updateCamera :: Camera -> StateT PlayState IO ()
updateCamera c = do
  statewit <- get
  put $ statewit {
    _camera = c
  }

move :: KeySet -> Time -> StateT PlayState IO ()
move keys time = do
  statewit <- get
  let Vertex2 x z = liftA2 (+)
          ((* (cast (head (time^.deltas))/1000)) <$> wayward keys)
          (statewit^.lily)
  newFrog <- lift $ setMeshTransform (head $ statewit^.meshes) (fromTranslation [x, -2, z - 5])

  put $ statewit {
      _lily = Vertex2 x z
       -- liftA2 (+)
       --   ((* (200 * throttle time)) <$> wayward keys)
       --   (statewit^.lily)
    , _meshes = hit 0 (const newFrog) $ statewit^.meshes
--     , _meshes = hit 0 ((\(Vertex2 x y) -> [x, -5, y]) (liftA2 (+)
--         ((* (200 * throttle time)) <$> wayward keys)
--         (statewit^.lily))) (statewit^.meshes)
  }
