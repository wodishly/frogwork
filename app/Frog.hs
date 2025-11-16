{- HLINT ignore "Use head" -}
module Frog (
  Frogwit (..)
, makeFrog
, moveFrog
, updateFrog
) where

import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), StateT, MonadTrans (lift))

import Numeric.LinearAlgebra ((!), fromList, toColumns, fromColumns)
import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3))

import Key (keyBegun, wasd)
import Mean (ssss)
import Rime (FrogVector, Point3, hat, (*^), (<+>), FrogVertex ((^*^)))
import Time (Timewit (lifetime), throttle)
import Allwit (Allwit (..))
import Shade (Mesh (meshAnimation), setMeshTransform)
import Matrix (frogLookAt)
import Data.Maybe (isJust, fromJust)
import Skeleton (play, evermore, once)
import FastenShade


data Frogwit = Frogwit {
    position :: Point3
  , dy :: GLfloat
  , speed :: GLfloat
  , aLeap :: GLfloat
  , weight :: GLfloat
  , leapCount :: Int
  , utleaps :: Int
  , mesh :: Mesh
  , didLeap :: Bool
  , didWalk :: Bool
}

makeFrog :: Mesh -> Frogwit
makeFrog m = Frogwit {
    position = Vertex3 0 0 0
  , dy = 0
  , speed = 2
  , aLeap = 5
  , weight = -8
  , leapCount = 0
  , utleaps = 2
  , mesh = m
  , didLeap = False
  , didWalk = False
}

didMove :: Frogwit -> Bool
didMove = ssss ((||) . didWalk) didLeap

hasLeapsLeft :: Frogwit -> Bool
hasLeapsLeft = ssss ((<) . leapCount) utleaps

leap :: Allwit -> StateT Frogwit IO ()
leap allwit = do
  frogwit <- get
  when (keyBegun (keyset allwit) ScancodeSpace && hasLeapsLeft frogwit) $
    put frogwit {
      dy = aLeap frogwit
    , leapCount = succ $ leapCount frogwit
    , didLeap = True
    }

fall :: Allwit -> StateT Frogwit IO ()
fall allwit = do
  frogwit <- get
  let Vertex3 x y z = position frogwit
      y' = y + throttle (timewit allwit) (dy frogwit)
  if y' <= 0
    then land
    else do
      put frogwit {
        dy = dy frogwit + throttle (timewit allwit) (weight frogwit)
      , position = Vertex3 x y' z
      }

land :: StateT Frogwit IO ()
land = do
  frogwit <- get
  put frogwit {
    dy = 0
  , leapCount = 0
  , position = Vertex3 1 0 1 ^*^ position frogwit
  , didLeap = False
  }

walk :: Allwit -> FrogVector -> StateT Frogwit IO ()
walk allwit forward = do
  frogwit <- get
  let direction = hat $ Vertex3 (forward!0) 0 -(forward!2)
      position' = position frogwit <+> (throttle (timewit allwit) (speed frogwit) *^ direction)

  if let Vertex2 _ dz = wasd (keyset allwit) in dz < 0
    then put frogwit { position = position', didWalk = True }
    else put frogwit { didWalk = False }

updateFrog :: Allwit -> FrogVector -> StateT Frogwit IO ()
updateFrog allwit forward = do
  frogwit <- get
  moveFrog allwit forward
  when (didMove frogwit) (moveMesh forward)
  animateMesh allwit

moveFrog :: Allwit -> FrogVector -> StateT Frogwit IO ()
moveFrog allwit forward = do
  walk allwit forward
  leap allwit
  fall allwit

moveMesh :: FrogVector -> StateT Frogwit IO ()
moveMesh forward = do
  frogwit <- get

  let (Vertex3 x y z) = position frogwit
      frogPosition = fromList [x, y, z]
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

  newFrogMesh <- lift $ setMeshTransform transform' (mesh frogwit)
  put frogwit { mesh = newFrogMesh }

animateMesh :: Allwit -> StateT Frogwit IO ()
animateMesh allwit = do
  frogwit <- get

  let frogMesh = mesh frogwit
      athem = meshAnimation frogMesh
      now = lifetime $ timewit allwit

  when (isJust athem) $
    let newAnimation = play now
          ((if didLeap frogwit then once else evermore) (fromJust athem))
          (if didLeap frogwit then BUNNY_JUMP else if didMove frogwit then BUNNY_WALK else BUNNY_IDLE)
        newFrogMesh = frogMesh { meshAnimation = Just newAnimation }
    in put frogwit { mesh = newFrogMesh }
