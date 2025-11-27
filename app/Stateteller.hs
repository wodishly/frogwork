module Stateteller
  ( module Stateteller,
    module Allwit,
  )
where

import Allwit
import Mean
import Shade
import State
import Stavework

import AboutState as About
import EndState as End
import PauseState as Pause
import PlayState as Play
import TitleState as Title
import WillState as Will


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

makeStateteller :: (Int, Int) -> Settings -> Meshlist -> Stateteller
makeStateteller (w, h) settings meshes = Stateteller
  (makeTitleState wind)
  (makeWillState  wind settings)
  (makePlayState  wind meshes)
  (makePauseState wind)
  (makeAboutState wind)
  makeEndState
  TitleName
  where wind = fromIntegral <$> Vertex2 w h

goto :: Stately a => Lens' Stateteller a -> Allwit -> StateT Stateteller IO Allwit
goto oldState oldWit = do
  teller <- get
  let oldName = nowState teller
      newName = name $ teller^.oldState

  wit' <- if newName /= oldName
    then do
      flushWritings
      lift $ execStateT (wakeState $ newName == PlayName) oldWit
    else return oldWit
  (newWit, newState) <- lift $ runStateT (loop wit') (teller^.oldState)
  updateState oldState newState newName
  return newWit

updateState :: Stately a => Lens' Stateteller a -> a -> StateName -> StateT Stateteller IO ()
updateState newState oldState newName = do
  teller <- get
  put $ (newState .~ oldState) teller { nowState = samely newName (name oldState) }

flushWritings :: StateT Stateteller IO ()
flushWritings = do
  teller <- get
  put teller {
    _titleState = flush Title.writings (teller^.titleState),
    _willState = flush Will.writings (teller^.willState),
    _playState = (teller^.playState) { speechframe = (teller^.playState).speechframe { writtens = Nothing } },
    _pauseState = flush Pause.writings (teller^.pauseState),
    _aboutState = flush About.writings (teller^.aboutState)
  } where flush = (%~ map (throoks %~ const Nothing))
