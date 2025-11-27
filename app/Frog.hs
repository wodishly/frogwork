{- HLINT ignore "Use head" -}
module Frog where

import Allwit
import FastenFrame
import FastenShade
import Key
import Matrix
import Mean
import Rime
import Shade
import Skeleton
import Strike
import Time


data Frogwit = Frogwit {
  position :: Point3,
  dy :: GLfloat,
  speed :: GLfloat,
  aLeap :: GLfloat,
  weight :: GLfloat,
  leapCount :: Int,
  utleaps :: Int,
  meshset :: Meshset,
--  hitframe :: Mesh,
  didLeap :: Bool,
  didWalk :: Bool,
  isRunning :: Bool
}

instance Spitful Frogwit where
  spit Frogwit { position } = twimap (<+> position) tallspit

makeFrog :: Meshset -> Frogwit
makeFrog meshset = Frogwit {
  position = Vertex3 0 0 0,
  dy = 0,
  speed = 2,
  aLeap = 5,
  weight = -8,
  leapCount = 0,
  utleaps = 2,
  meshset,
  didLeap = False,
  didWalk = False,
  isRunning =  False
}

didMove :: Frogwit -> Bool
didMove = anyIn [didWalk, didLeap]

hasLeapsLeft :: Frogwit -> Bool
hasLeapsLeft Frogwit { leapCount, utleaps } = leapCount < utleaps

run :: Allwit -> StateT Frogwit IO ()
run Allwit { keyset } = do
  frogwit <- get
  let isRunning = anyKeysContinuing keyset [ScancodeLShift, ScancodeRShift]
  put frogwit {
    speed = if isRunning then 8 else 2,
    isRunning 
  }

leap :: Allwit -> StateT Frogwit IO ()
leap Allwit { keyset } = do
  frogwit@Frogwit { aLeap, leapCount } <- get

  when (keyBegun keyset ScancodeSpace && hasLeapsLeft frogwit) $
    put frogwit {
      dy = aLeap,
      leapCount = succ leapCount,
      didLeap = True
    }

fall :: Allwit -> StateT Frogwit IO ()
fall Allwit { timewit } = do
  frogwit@Frogwit { position = Vertex3 x y z, dy, weight } <- get
  let y' = y + throttle timewit dy
  if y' <= 0
    then land
    else put frogwit {
      dy = dy + throttle timewit weight,
      position = Vertex3 x y' z
    }

land :: StateT Frogwit IO ()
land = do
  frogwit@Frogwit {
    position = Vertex3 x _ z
  } <- get
  put frogwit {
    dy = 0,
    leapCount = 0,
    position = Vertex3 x 0 z,
    didLeap = False
  }

walk :: Allwit -> FrogVector -> StateT Frogwit IO ()
walk Allwit { keyset, timewit } forward = do
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

stirshift :: FrogVector -> FrogVector -> FrogMatrix
stirshift start end = let
  end' = start + fromList [end!0, 0, end!2]
  trans = frogLookAt start end'
  columns = toColumns trans
  in fromColumns [
    columns!!0,
    columns!!1,
    columns!!2,
    fromList $ toList start ++ [1]
  ]

moveMesh :: FrogVector -> StateT Frogwit IO ()
moveMesh forward = do
  frogwit@Frogwit { position = Vertex3 x y z, meshset = Meshset { main, hitframe } } <- get

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

  frogFrame <- lift $ setMeshTransform (shapeshiftFrame $ spit frogwit) =<< setMeshTransform transform' hitframe
  newFrogMesh <- lift $ setMeshTransform transform' main
  put frogwit { meshset = Meshset { main = newFrogMesh, hitframe = frogFrame } }

animateMesh :: Allwit -> StateT Frogwit IO ()
animateMesh (Allwit { timewit = Timewit { lifetime } }) = do
  frogwit@Frogwit { meshset = meshset@Meshset { main }, didLeap, isRunning } <- get

  let athem = meshAnimation main

  when (isJust athem) $
    let
    what
      | didLeap = BUNNY_JUMP
      | didMove frogwit && isRunning  = BUNNY_RUN
      | didMove frogwit  = BUNNY_WALK
      | otherwise = BUNNY_IDLE
    how
      | didLeap = once
      | otherwise = evermore
    newAnimation = play lifetime (how (fromJust athem)) what
    in put frogwit { meshset = meshset { main = main { meshAnimation = Just newAnimation } } }
