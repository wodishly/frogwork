{- HLINT ignore "Use head" -}
module Telly where

import Allwit
import Rime
import Shade
import State
import Strike hiding (spit)


data Tellywit = Tellywit {
  position :: Point3,
  spit :: Spit,
  meshset :: Meshset
}

updateTelly :: Allwit -> StateT Tellywit IO ()
updateTelly _ = do
  tellywit@Tellywit { spit, meshset = meshset@Meshset { hitframe } } <- get
  hitframe' <- lift $ setMeshTransform (shapeshiftFrame spit) hitframe
  put tellywit { meshset = meshset { hitframe = hitframe' } }

makeTelly :: Meshset -> Tellywit
makeTelly meshset = Tellywit {
  position = Vertex3 -2 0 2,
  spit = (Vertex3 -2 0 2, Vertex3 0 2 4),
  meshset
}
