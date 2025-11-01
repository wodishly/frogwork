{-# OPTIONS_GHC -Wno-name-shadowing #-}

module PlayState where

import Control.Lens
import Foreign

import Graphics.Rendering.OpenGL as GL

import State
import Light
import Key
import Time
  
playState :: GameState
playState _ctx _keys _events stateInfo = do
  stateInfo <- move stateInfo
  bg black

  lilyPtr <- new (stateInfo^.lily)
  uniformv (stateInfo^.uloc) 1 lilyPtr

  activeTexture $= TextureUnit 0
  tptr <- new (TextureUnit 0)
  _ <- uniformv (stateInfo^.tloc) 1 tptr

  drawFaces 1800
  return stateInfo

move :: StateInfo -> IO StateInfo
move stateInfo = pure $ set lily lily' stateInfo where
  lily' = liftA2 (+)
    ((* (200 * throttle (stateInfo^.time))) <$> wayward (stateInfo^.keyset))
    (stateInfo^.lily)