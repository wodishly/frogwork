{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PlayState where

import Control.Lens
import Foreign (new)
import Graphics.Rendering.OpenGL as GL

import State
import Light
import Key
import Time

playState :: GameState
playState _ctx _keys _events stateInfo = do
  stateInfo <- move stateInfo
  bg black

  currentProgram $= Just ((fst.last) (stateInfo^.programs))
  bindVertexArrayObject $= Just ((snd.last) (stateInfo^.programs))
  drawFaces 6

  currentProgram $= Just ((fst.head) (stateInfo^.programs))
  bindVertexArrayObject $= Just ((snd.head) (stateInfo^.programs))
  lilyPtr <- new (stateInfo^.lily)
  uniformv (stateInfo^.uloc) 1 lilyPtr

  activeTexture $= TextureUnit 0
  tptr <- new (TextureUnit 0)
  uniformv (stateInfo^.tloc) 1 tptr

  drawFaces 1800

  return stateInfo

move :: StateInfo -> IO StateInfo
move stateInfo = pure $ set lily lily' stateInfo where
  lily' = liftA2 (+)
    ((* (200 * throttle (stateInfo^.time))) <$> wayward (stateInfo^.keyset))
    (stateInfo^.lily)