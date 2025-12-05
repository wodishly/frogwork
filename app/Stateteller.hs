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
  nowState :: StateName,
  _titleState :: TitleState,
  _willState :: WillState,
  _playState :: PlayState,
  _pauseState :: PauseState,
  _aboutState :: AboutState,
  _endState :: EndState
}
makeLenses ''Stateteller

makeStateteller :: (Int, Int) -> Allwit -> (Allwit, Stateteller)
makeStateteller (w, h) allwit =
  let window = fromIntegral <$> Vertex2 w h
      (allwit', madePlayState) = makePlayState allwit
  in (allwit', Stateteller TitleName
    (makeTitleState window)
    (makeWillState  window allwit)
    madePlayState
    (makePauseState window)
    (makeAboutState window)
    makeEndState)

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
    _titleState = floosh Title.writings (teller^.titleState),
    _willState = floosh Will.writings (teller^.willState),
    _playState = (teller^.playState) { Play.speechframe = (teller^.playState).speechframe { writtens = Nothing } },
    _pauseState = floosh Pause.writings (teller^.pauseState),
    _aboutState = floosh About.writings (teller^.aboutState)
  } where floosh = (%~ map flush)
