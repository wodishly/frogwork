{-# OPTIONS_GHC -Wno-name-shadowing #-}
module MenuState where

import State
import Light
import Stave
import Control.Monad
import Control.Lens
import SDL hiding (Play)
import Mean

menuOptions :: [String]
menuOptions = ["play", "frog", "quit"]

drawTitle :: Renderer -> Feather -> IO ()
drawTitle renderer feather = do
  feather <- pure $ set featherColor blue feather
  drawWord renderer feather (V2 (centeredX feather "welcome to") 50) "welcome to"
  drawWord renderer feather (V2 (centeredX feather "frogford") 150) "frogford"

menuState :: GameState
menuState renderer _keys _events stateInfo = do
  bg renderer (clerp (1/4) white)
  drawTitle renderer (stateInfo^.feather)
  forM_ (zip [0..] menuOptions) (\(i, choice) -> do
    drawWord renderer (
      if i == stateInfo^.menuFinger
      then set featherColor magenta (stateInfo^.feather)
      else stateInfo^.feather
      ) (V2 (centeredX (stateInfo^.feather) choice) (200 + 100*(cast.succ) i)) choice)
  return stateInfo
