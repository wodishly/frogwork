{-# LANGUAGE DuplicateRecordFields #-}
{- HLINT ignore "Use head" -}
module Frog (
  Frogwit (..),
  makeFrog,
  moveFrog,
  updateFrog,
) where

import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT)
import Data.Maybe (isJust, fromJust)

import Numeric.LinearAlgebra (fromColumns, fromList, toColumns, (!))
import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2), Vertex3 (Vertex3))
import SDL.Input.Keyboard.Codes

import Allwit (Allwit (..))
import FastenShade
import Key (keyBegun, wasd, anyKeysContinuing)
import Matrix (frogLookAt, fromAffine)
import Rime (FrogVector, Point3, hat, (*^), (<+>), FrogVertex (toFrogList))
import Shade (Mesh (meshAnimation), setMeshTransform)
import Skeleton (evermore, once, play)
import Time (Timewit (lifetime, Timewit), throttle)
import Mean (anyIn, twimap, thrice)
import Strike (Spitful (..))
import FastenFrame (onespit)


data Frogwit = Frogwit {
  position :: Point3,
  dy :: GLfloat,
  speed :: GLfloat,
  aLeap :: GLfloat,
  weight :: GLfloat,
  leapCount :: Int,
  utleaps :: Int,
  mesh :: Mesh,
  fresh :: Mesh,
  didLeap :: Bool,
  didWalk :: Bool,
  isRunning :: Bool
}

instance Spitful Frogwit where
  spit Frogwit { position } = twimap (<+> position) onespit

makeFrog :: Mesh -> Mesh -> Frogwit
makeFrog m fresh = Frogwit {
  position = Vertex3 0 0 0,
  dy = 0,
  speed = 2,
  aLeap = 5,
  weight = -8,
  leapCount = 0,
  utleaps = 2,
  mesh = m,
  fresh = fresh,
  didLeap = False,
  didWalk = False,
  isRunning =  False
}

didMove :: Frogwit -> Bool
didMove = anyIn [didWalk, didLeap]

hasLeapsLeft :: Frogwit -> Bool
hasLeapsLeft (Frogwit { leapCount, utleaps }) = leapCount < utleaps

run :: Allwit -> StateT Frogwit IO ()
run (Allwit { keyset }) = do
  frogwit <- get
  let isRunning = anyKeysContinuing keyset [ScancodeLShift, ScancodeRShift]
  put frogwit {
    speed = if isRunning then 8 else 2,
    isRunning 
  }

leap :: Allwit -> StateT Frogwit IO ()
leap (Allwit { keyset }) = do
  frogwit@Frogwit { aLeap, leapCount } <- get
  when (keyBegun keyset ScancodeSpace && hasLeapsLeft frogwit) $
    put frogwit {
      dy = aLeap,
      leapCount = succ leapCount,
      didLeap = True
    }

fall :: Allwit -> StateT Frogwit IO ()
fall (Allwit { timewit }) = do
  frogwit@Frogwit { position = Vertex3 x y z, dy, weight } <- get
  let y' = y + throttle timewit dy
  if y' <= 0
    then land
    else do
      put frogwit {
        dy = dy + throttle timewit weight
      , position = Vertex3 x y' z
      }

land :: StateT Frogwit IO ()
land = do
  frogwit@Frogwit { position = Vertex3 x _ z } <- get
  put frogwit {
    dy = 0
  , leapCount = 0
  , position = Vertex3 x 0 z
  , didLeap = False
  }

walk :: Allwit -> FrogVector -> StateT Frogwit IO ()
walk (Allwit { keyset, timewit }) forward = do
  frogwit@Frogwit { position, speed } <- get
  let direction = hat $ Vertex3 (forward!0) 0 -(forward!2)
      position' = position <+> (throttle timewit speed *^ direction)
      Vertex2 _ dz = wasd keyset

  if dz < 0
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
  run allwit
  walk allwit forward
  leap allwit
  fall allwit

moveMesh :: FrogVector -> StateT Frogwit IO ()
moveMesh forward = do
  frogwit@Frogwit { position = Vertex3 x y z, mesh, fresh } <- get

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

  frogFrame <- lift $ setMeshTransform (fromAffine [1, 2, 1] (zipWith (+) [-0.5, 0.1, -0.5] (toFrogList $ fst $ spit frogwit))) =<< setMeshTransform transform' fresh
  newFrogMesh <- lift $ setMeshTransform transform' mesh
  put frogwit { mesh = newFrogMesh, fresh = frogFrame }


animateMesh :: Allwit -> StateT Frogwit IO ()
animateMesh (Allwit { timewit = Timewit { lifetime } }) = do
  frogwit@Frogwit { mesh, didLeap, isRunning } <- get

  let athem = meshAnimation mesh

  when (isJust athem) $
    let newAnimation = play lifetime
          ((if didLeap then once else evermore) (fromJust athem))
          (if didLeap
            then BUNNY_JUMP
            else if didMove frogwit
              then if isRunning
                then BUNNY_RUN
                else BUNNY_WALK
              else BUNNY_IDLE)
    in put frogwit { mesh = mesh { meshAnimation = Just newAnimation } }
