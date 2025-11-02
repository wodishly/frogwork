{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PlayState where

import Control.Lens
import Foreign (new)
import Graphics.Rendering.OpenGL as GL
import qualified Data.HashMap.Strict as HM
import SDL.Vect
import Data.Maybe
import Rime (cast)
import SDL (windowSize)

import State
import Light
import Key
import Time
import Shade

playState :: GameState
playState _ctx _keys _events stateInfo = do
  stateInfo <- move stateInfo
  bg black

  let w = fromJust (stateInfo^.window)
  V2 windowWidth windowHeight <- (cast <$>) <$> get (windowSize w)
  viewport $= (Position 0 0, Size windowWidth windowHeight)
  let aspect = fromIntegral windowWidth / fromIntegral windowHeight :: Float
  let display = RenderView {
    aspect = aspect,
    fov = pi / 4.0,
    near = 0.1,
    far = 100.0
  }
  let projectionMatrix = getProjectionMatrix display

  -- mesh rendering --
  let m = stateInfo^.meshes
  updatePlayerUniforms stateInfo
  mapM_ (`drawMesh` projectionMatrix) m

  return stateInfo

updatePlayerUniforms :: StateInfo -> IO ()
updatePlayerUniforms stateInfo = do
  -- assume player = first mesh
  let player = head (stateInfo^.meshes)
  useMesh player
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