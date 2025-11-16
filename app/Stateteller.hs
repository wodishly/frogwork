{-# LANGUAGE ImpredicativeTypes #-}
module Stateteller where

import Control.Lens (Lens', makeLenses, (.~), (^.))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT), execStateT)

import SDL.Input.Keyboard.Codes

import Allwit (Allwit (..), Settings, setWindowGrabbed)
import State (StateName (..), Stately (loop, name))
import TitleState (TitleState, makeTitleState)
import WillState (WillState, makeWillState)
import PlayState (PlayState, makePlayState)
import PauseState (PauseState, makePauseState)
import EndState (EndState, makeEndState)

import qualified TitleState as Title (chosen)
import qualified WillState as Will (chosen)

import Key (anyKeysBegun, keyBegun)
import Matrix (RenderView)
import Shade (Mesh)
import Data.Maybe (isNothing)


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
  let wit = frogwork^.allwit
  let keys = keyset wit
      newState
        | keyBegun keys ScancodeR = TitleName
        | nowState (frogwork^.stateteller) == PlayName && keyBegun keys ScancodeP = PauseName
        | nowState (frogwork^.stateteller) == PauseName && keyBegun keys ScancodeP = PlayName
        | nowState (frogwork^.stateteller) == TitleName && keyBegun keys ScancodeReturn = Title.chosen (frogwork^.stateteller.titleState)
        | nowState (frogwork^.stateteller) == WillName && keyBegun keys ScancodeReturn && isNothing (Will.chosen $ frogwork^.stateteller.willState) = TitleName
        | otherwise = nowState (frogwork^.stateteller)
  
  (wit', teller) <- lift (runStateT (case newState of
        TitleName -> goto titleState wit
        WillName -> goto willState wit
        PlayName -> goto playState wit
        PauseName -> goto pauseState wit
        EndName -> goto endState wit
      ) (frogwork^.stateteller)
    )
  put $ Frogwork wit' teller

goto :: Stately a => Lens' Stateteller a -> Allwit -> StateT Stateteller IO Allwit
goto lens wit = do
  teller <- get
  wit' <- lift $ execStateT (setWindowGrabbed $ name (teller^.lens) == PlayName) wit
  (wit'', state') <- lift $ runStateT (loop wit') (teller^.lens)
  put $ (lens.~state') teller { nowState = name state' }
  return wit''
