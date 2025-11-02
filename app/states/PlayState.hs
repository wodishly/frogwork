{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PlayState where

import Control.Lens
import Foreign (new)
import Graphics.Rendering.OpenGL as GL
import qualified Data.HashMap.Strict as HM

import State
import Light
import Key
import Time
import Shade

playState :: GameState
playState _ctx _keys _events stateInfo = do
  stateInfo <- move stateInfo
  bg black

  -- currentProgram $= Just ((fst.last) (stateInfo^.programs))
  -- bindVertexArrayObject $= Just ((snd.last) (stateInfo^.programs))
  -- drawFaces 6

  let m = stateInfo^.meshes
  useMesh $ head m
  updatePlayerUniforms stateInfo
  mapM_ drawMesh m

  return stateInfo

updatePlayerUniforms :: StateInfo -> IO ()
updatePlayerUniforms stateInfo = do
  -- assume player = first mesh
  let player = head (stateInfo^.meshes)
  let uniforms = uniformMap player

  -- is `new` appropriate here? 
  lilyPtr <- new (stateInfo^.lily)
  inputLocation <- uniforms HM.! "u_input2d"
  uniformv inputLocation 1 lilyPtr


move :: StateInfo -> IO StateInfo
move stateInfo = pure $ set lily lily' stateInfo where
  lily' = liftA2 (+)
    ((* (200 * throttle (stateInfo^.time))) <$> wayward (stateInfo^.keyset))
    (stateInfo^.lily)