module Stateteller where

import Control.Lens (Lens', makeLenses, (.~), (^.))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT), execStateT)

import SDL.Input.Keyboard.Codes

import Allwit (Allwit (..), Settings, setWindowGrabbed)
import State (StateName (..), Stately (loop, name))
import TitleState (TitleState, makeTitleState, chosen)
import WillState (WillState, makeWillState)
import PlayState (PlayState, makePlayState)
import PauseState (PauseState, makePauseState)
import EndState (EndState, makeEndState)

import Key (anyKeysBegun, keyBegun)
import Matrix (RenderView)
import Shade (Mesh)


data Stateteller = Stateteller {
  _titleState :: TitleState
, _willState :: WillState
, _playState :: PlayState
, _pauseState :: PauseState
, _endState :: EndState
, nowState :: StateName
}
makeLenses ''Stateteller

makeStateteller :: RenderView -> Settings -> [Mesh] -> Stateteller
makeStateteller dis sets meshes = Stateteller
  (makeTitleState dis)
  (makeWillState dis sets)
  (makePlayState dis meshes)
  (makePauseState dis)
  makeEndState
  TitleName

data Frogwork = Frogwork {
  _allwit :: Allwit
, _stateteller :: Stateteller
}
makeLenses ''Frogwork

didEnd :: Frogwork -> Bool
didEnd frogwork =
    nowState (frogwork^.stateteller) == EndName
 || anyKeysBegun (keyset $ frogwork^.allwit) [ScancodeQ, ScancodeEscape]

settleState :: StateT Frogwork IO ()
settleState = do
  frogwork <- get
  let keys = keyset $ frogwork^.allwit
      newState
        | keyBegun keys ScancodeR = TitleName
        | nowState (frogwork^.stateteller) == PlayName && keyBegun keys ScancodeP = PauseName
        | nowState (frogwork^.stateteller) == PauseName && keyBegun keys ScancodeP = PlayName
        | nowState (frogwork^.stateteller) == TitleName && keyBegun keys ScancodeReturn = chosen (frogwork^.stateteller.titleState)
        | otherwise = nowState (frogwork^.stateteller)

  wit' <- lift $ execStateT (setWindowGrabbed (newState == PlayName)) (frogwork^.allwit)
  (wit'', teller) <- lift (runStateT (case newState of
        TitleName -> goto wit' titleState
        WillName -> goto wit' willState
        PlayName -> goto wit' playState
        PauseName -> goto wit' pauseState
        EndName -> goto wit' endState
      ) (frogwork^.stateteller)
    )
  put $ Frogwork wit'' teller

goto :: Stately a => Allwit -> Lens' Stateteller a -> StateT Stateteller IO Allwit
goto wit lens = do
  teller <- get
  (wit', state) <- lift $ runStateT (loop wit) (teller^.lens)
  put $ (lens.~state) teller { nowState = name state }
  return wit'
