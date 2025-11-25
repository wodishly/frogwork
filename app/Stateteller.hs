{- HLINT ignore "Use section" -}
module Stateteller where

import Control.Lens (Lens', makeLenses, (%~), (.~), (^.))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT), execStateT)

import Graphics.Rendering.OpenGL (Vertex2 (Vertex2))

import Allwit
import State
import TitleState as Title
import WillState as Will
import PlayState as Play
import PauseState as Pause
import AboutState as About
import EndState as End

import Mean
import Stavework


data Stateteller = Stateteller {
  _titleState :: TitleState,
  _willState :: WillState,
  _playState :: PlayState,
  _pauseState :: PauseState,
  _aboutState :: AboutState,
  _endState :: EndState,
  nowState :: StateName
}
makeLenses ''Stateteller

makeStateteller :: (Int, Int) -> Settings -> UnholyMeshMash -> Stateteller
makeStateteller (w, h) sets meshes = Stateteller
  (makeTitleState wind)
  (makeWillState  wind sets)
  (makePlayState  wind meshes)
  (makePauseState wind)
  (makeAboutState wind)
  makeEndState
  TitleName
  where wind = fromIntegral <$> Vertex2 w h

goto :: Stately a => Lens' Stateteller a -> Allwit -> StateT Stateteller IO Allwit
goto lens wit = do
  teller <- get
  let oldName = nowState teller
      newName = name $ teller^.lens

  wit' <- if newName /= oldName
    then do
      flushWritings
      lift $ execStateT (wakeState $ newName == PlayName) wit
    else return wit
  (wit'', state) <- lift $ runStateT (loop wit') (teller^.lens)
  updateState lens state newName
  return wit''

updateState :: Stately a => Lens' Stateteller a -> a -> StateName -> StateT Stateteller IO ()
updateState lens state newName = do
  teller <- get
  put $ (lens.~state) teller { nowState = samely newName (name state) }

flushWritings :: StateT Stateteller IO ()
flushWritings = do
  teller <- get
  put teller {
    _titleState = flush Title.writings (teller^.titleState)
  , _willState = flush Will.writings (teller^.willState)
  , _playState = (teller^.playState) { speechframe = (teller^.playState).speechframe { writtens = Nothing } }
  , _pauseState = flush Pause.writings (teller^.pauseState)
  , _aboutState = flush About.writings (teller^.aboutState)
  } where flush = (%~ map (throoks %~ const Nothing))
