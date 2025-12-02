{- HLINT ignore "Use section" -}
module Frogwork where

import Prelude hiding (lookup)

import qualified SDL (glSwapWindow, pollEvents)
import qualified SDL.Time as SDL (ticks)

import Allwit
import Happen
import Key
import Mean
import Rime
import State
import Stateteller
import Time

-- import qualified TitleState as Title (chosen)
-- import qualified WillState as Will (chosen)


data Frogwork = Frogwork {
  allwit :: Allwit,
  stateteller :: Stateteller
}

didEnd :: StateT Frogwork IO Bool
didEnd = do
  Frogwork {
    allwit = Allwit { keyset },
    stateteller = Stateteller { nowState }
  } <- get
  return $ nowState == EndName || anyKeysBegun keyset [ScancodeQ, ScancodeEscape]

become :: StateT Frogwork IO ()
become = get >>= SDL.glSwapWindow . window . allwit

choose :: StateT Frogwork IO ()
choose = do
  Frogwork {
    allwit = allwit@Allwit { keyset },
    stateteller = stateteller@Stateteller { nowState }
  } <- get

  let
    newState
      | keyBegun keyset ScancodeR
        = TitleName

      | nowState == PlayName
        && keyBegun keyset ScancodeP
        = PauseName

      | nowState == PauseName
        && keyBegun keyset ScancodeP
        = PlayName

      | nowState == TitleName
        && keyBegun keyset ScancodeReturn
        = chosen (stateteller^.titleState)

      | nowState == WillName
        && keyBegun keyset ScancodeReturn
        && isNothing (chosen $ stateteller^.willState)
        = TitleName

      | nowState == AboutName
        && anyKeysBegun keyset (filter (nas [ScancodeQ, ScancodeEscape]) hearableKeys)
        = TitleName

      | otherwise = nowState
    in lift (runStateT (case newState of
          TitleName -> goto titleState allwit
          WillName -> goto willState allwit
          PlayName -> goto playState allwit
          PauseName -> goto pauseState allwit
          AboutName -> goto aboutState allwit
          EndName -> goto endState allwit
        ) stateteller
      ) >>= put . uncurry Frogwork

listen :: StateT Frogwork IO ()
listen = do
  listenEvents -- call me before @listenKeys@, for i churn the eventlist
  listenTime
  listenKeys
  listenMouse
  listenWindow
  listenAnswer

listenAnswer :: StateT Frogwork IO ()
listenAnswer = do
  frogwork@Frogwork { allwit } <- get
  wit <- lift $ execStateT answer allwit
  put frogwork { allwit = wit }

listenEvents :: StateT Frogwork IO ()
listenEvents = do
  frogwork@Frogwork { allwit } <- get
  es <- lift SDL.pollEvents
  put frogwork { allwit = allwit { events = es } }

listenTime :: StateT Frogwork IO ()
listenTime = do
  frogwork@Frogwork { allwit = allwit@Allwit { timewit } } <- get
  now <- lift SDL.ticks
  put frogwork { allwit = allwit { timewit = keepTime timewit (fromIntegral now) } }

listenKeys :: StateT Frogwork IO ()
listenKeys = do
  frogwork@Frogwork { allwit = allwit@Allwit { events, keyset } } <- get
  put frogwork { allwit = allwit { keyset = bethinkKeys events keyset } }

listenMouse :: StateT Frogwork IO ()
listenMouse = do
  frogwork@Frogwork { allwit = allwit@Allwit { events } } <- get
  -- todo sum instead of head
  let f x = if full x then head x else Vertex2 0 0
      mouse = twimap f (doBoth unwrapHappenPointer unwrapHappenWheel events)
    in put frogwork { allwit = allwit { mouse = uncurry Mousewit mouse } }

listenWindow :: StateT Frogwork IO ()
listenWindow = do
  frogwork@Frogwork { allwit = allwit@Allwit { events, window }, stateteller } <- get
  when (or $ unwrapHappenWindow events) $ do
    dis <- lift $ waxwane window
    teller' <- lift $ execStateT flushWritings stateteller
    put frogwork {
      allwit = allwit { display = dis },
      stateteller = teller'
    }
