module Stateteller (
  settleState
, didEnd
, makeStateteller
, allwit
, Frogwork (..)
) where

import Control.Lens (Lens', makeLenses, (.~), (^.), (%~))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT), execStateT)
import Data.Maybe (isNothing)

import SDL.Input.Keyboard.Codes
import Graphics.Rendering.OpenGL (Vertex2 (Vertex2))

import Allwit (Allwit (..), Settings, UnholyMeshMash, doStateSwitchStuff)
import State (StateName (..), Stately (loop, name))
import TitleState (TitleState, makeTitleState, writings)
import WillState (WillState, makeWillState)
import PlayState (PlayState, makePlayState)
import PauseState (PauseState, makePauseState)
import AboutState (AboutState, makeAboutState)
import EndState (EndState, makeEndState)

import qualified TitleState as Title (chosen)
import qualified WillState as Will (chosen)

import Key (anyKeysBegun, keyBegun, hearableKeys)
import Mean (samely, preent)
import Stavework (throoks)
import Control.Monad (when)


data Stateteller = Stateteller {
  _titleState :: TitleState
, _willState :: WillState
, _playState :: PlayState
, _pauseState :: PauseState
, _aboutState :: AboutState
, _endState :: EndState
, nowState :: StateName
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
      keys = keyset wit
      teller = frogwork^.stateteller
      newState
        | keyBegun keys ScancodeR = TitleName
        | nowState teller == PlayName && keyBegun keys ScancodeP = PauseName
        | nowState teller == PauseName && keyBegun keys ScancodeP = PlayName
        | nowState teller == TitleName && keyBegun keys ScancodeReturn = Title.chosen (frogwork^.stateteller.titleState)
        | nowState teller == WillName && keyBegun keys ScancodeReturn && isNothing (Will.chosen $ frogwork^.stateteller.willState) = TitleName
        | nowState teller == AboutName && anyKeysBegun keys (filter (flip notElem [ScancodeQ, ScancodeEscape]) hearableKeys) = TitleName
        | otherwise = nowState teller

  lift (runStateT (case newState of
        TitleName -> goto titleState wit
        WillName -> goto willState wit
        PlayName -> goto playState wit
        PauseName -> goto pauseState wit
        AboutName -> goto aboutState wit
        EndName -> goto endState wit
      ) (frogwork^.stateteller)
    ) >>= put . uncurry Frogwork

goto :: Stately a => Lens' Stateteller a -> Allwit -> StateT Stateteller IO Allwit
goto lens wit = do
  teller <- get
  -- let oldName = nowState teller
  --     newName = name $ teller^.lens
  -- (wit'', state) <- stuff lens wit

  let oldName = nowState teller
      newName = name $ teller^.lens

  wit' <- (if newName /= oldName
    then lift . execStateT (doStateSwitchStuff $ newName == PlayName)
    else return) wit
  (wit'', state) <- lift $ runStateT (loop wit') (teller^.lens)
  when (oldName /= newName) flushWritings
  swappy lens state newName
  return wit''

stuff :: Stately a => Lens' Stateteller a -> Allwit -> StateT Stateteller IO (Allwit, a)
stuff lens wit = do
  teller <- get
  let oldName = nowState teller
      newName = name $ teller^.lens

  wit' <- (if newName /= oldName
    then lift . execStateT (doStateSwitchStuff $ newName == PlayName)
    else return) wit
  lift $ runStateT (loop wit') (teller^.lens)

swappy :: Stately a => Lens' Stateteller a -> a -> StateName -> StateT Stateteller IO ()
swappy lens state newName = do
  teller <- get
  put $ (lens.~state) teller { nowState = samely newName (name state) }

flushWritings :: StateT Stateteller IO ()
flushWritings = do
  teller <- get
  put teller { _titleState = (writings %~ map (throoks %~ const Nothing)) (teller^.titleState) }
  preent $ head $ map (^.throoks) $ teller^.titleState.writings
