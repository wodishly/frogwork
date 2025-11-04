{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PlayState where

import Control.Lens
import Graphics.Rendering.OpenGL as GL
import SDL.Vect
import Data.Maybe
import Rime (cast)
import SDL (windowSize)

import FrogState
import Light
import Key
import Time
import Shade
import Matrix

playState :: GameState
playState _ctx _keys stateInfo = do
  stateInfo <- move stateInfo
  bg black

  let w = fromJust (stateInfo^.window)
  V2 windowWidth windowHeight <- (cast <$>) <$> get (windowSize w)
  viewport $= (Position 0 0, Size windowWidth windowHeight)
  let aspect = fromIntegral windowWidth / fromIntegral windowHeight :: Float
  let display = RenderView {
    _aspect = aspect,
    _fov = pi / 4.0,
    _near = 0.1,
    _far = 100.0
  }
  let projectionMatrix = getProjectionMatrix display

  let Vertex2 a b = stateInfo^.lily
  stateInfo <- pure $ set camera (Camera {
      cTarget=[cos(pi/2+a/10000),-1,sin(pi/2+a/10000)-1+b/10000],
      cPosition=[0,-1,-1+b/10000]
    }) stateInfo
  let c = stateInfo^.camera
  let viewMatrix = frogLookAt (cPosition c) (cTarget c)
  -- print viewMatrix
  -- let lapl = detLaplace $ unhew viewMatrix
  -- print lapl

  -- mesh rendering --
  let m = stateInfo^.meshes
  mapM_ (\mesh -> drawMesh mesh projectionMatrix viewMatrix) m

  return stateInfo

move :: StateWit -> IO StateWit
move stateInfo = pure $ set lily lily' stateInfo where
  lily' = liftA2 (+)
    ((* (200 * throttle (stateInfo^.time))) <$> wayward (stateInfo^.keyset))
    (stateInfo^.lily)
