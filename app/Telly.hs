{- HLINT ignore "Use head" -}
module Telly where

import Allwit
import Rime
import Shade
import Strike
import Matrix


data Tellywit = Tellywit {
  position :: Point3,
  spit :: Spit,
  meshset :: Meshset
}

updateTelly :: Allwit -> StateT Tellywit IO ()
updateTelly _ = do return ()
  -- tellywit@Tellywit { spit, meshset = meshset@Meshset { hitframe } } <- get
  -- hitframe' <- lift $ setMeshTransform (shapeshiftFrame spit) hitframe
  -- put tellywit { meshset = meshset { hitframe = hitframe' } }

makeTelly :: Point3 -> Spit -> Meshset -> Tellywit
makeTelly position@(Vertex3 x y z) spit@(Vertex3 lx ly lz, _) Meshset { main, hitframe } = Tellywit {
  position,
  spit,
  meshset = Meshset {
    main = setMeshTransform (fromTranslation [x+lx, y+ly+1, z+lz]) main,
    hitframe = setMeshTransform (shapeshiftFrame spit position) hitframe
  }
}
