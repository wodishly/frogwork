{- HLINT ignore "Use section" -}
module Frogwork (
  Frogwork (..),
  listen,
  settleState,
  didEnd,
  waxwane
) where

import Prelude hiding (lookup)

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT), execStateT)
import Data.Maybe (isNothing)

import Graphics.Rendering.OpenGL (
    BlendingFactor (OneMinusSrcAlpha, SrcAlpha)
  , Capability (Enabled)
  , ComparisonFunction (Lequal)
  , HasSetter (($=))
  , Position (Position)
  , Size (Size)
  )
import SDL (pollEvents, V2 (V2), Window, glGetDrawableSize)
import SDL.Input.Keyboard.Codes

import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL.Time as SDL

import Allwit (Allwit (..), answer)
import State (StateName (..))
import Stateteller (Stateteller (nowState), aboutState, endState, flushWritings, goto, pauseState, playState, titleState, willState)

import qualified TitleState as Title (chosen)
import qualified WillState as Will (chosen)

import Happen (Mousewit (Mousewit), unwrapHappenPointer, unwrapHappenWheel, unwrapHappenWindow)
import Key (anyKeysBegun, hearableKeys, keyBegun, bethinkKeys)
import Mean (doBoth, full, twimap)
import Time (keepTime)
import Matrix (RenderView(..))


data Frogwork = Frogwork {
  allwit :: Allwit,
  stateteller :: Stateteller
}

didEnd :: Frogwork -> Bool
didEnd frogwork =
    nowState (stateteller frogwork) == EndName
 || anyKeysBegun (keyset $ allwit frogwork) [ScancodeQ, ScancodeEscape]

settleState :: StateT Frogwork IO ()
settleState = do
  frogwork <- get
  let wit = allwit frogwork
      keys = keyset wit
      teller = stateteller frogwork
      newState
        | keyBegun keys ScancodeR = TitleName
        | nowState teller == PlayName && keyBegun keys ScancodeP = PauseName
        | nowState teller == PauseName && keyBegun keys ScancodeP = PlayName
        | nowState teller == TitleName && keyBegun keys ScancodeReturn = Title.chosen (stateteller frogwork^.titleState)
        | nowState teller == WillName && keyBegun keys ScancodeReturn && isNothing (Will.chosen $ stateteller frogwork^.willState) = TitleName
        | nowState teller == AboutName && anyKeysBegun keys (filter (flip notElem [ScancodeQ, ScancodeEscape]) hearableKeys) = TitleName
        | otherwise = nowState teller
    in lift (runStateT (case newState of
          TitleName -> goto titleState wit
          WillName -> goto willState wit
          PlayName -> goto playState wit
          PauseName -> goto pauseState wit
          AboutName -> goto aboutState wit
          EndName -> goto endState wit
        ) (stateteller frogwork)
      ) >>= put . uncurry Frogwork

listen :: StateT Frogwork IO ()
listen = do
  listenEvents
  listenTime
  listenKeys
  listenMouse
  listenWindow
  listenAnswer

listenAnswer :: StateT Frogwork IO ()
listenAnswer = do
  frogwork <- get
  wit <- lift $ execStateT answer (allwit frogwork)
  put frogwork { allwit = wit }

listenEvents :: StateT Frogwork IO ()
listenEvents = do
  frogwork <- get
  es <- lift SDL.pollEvents
  let wit = allwit frogwork
    in put frogwork { allwit = wit { events = es } }

listenTime :: StateT Frogwork IO ()
listenTime = do
  frogwork <- get
  now <- lift $ fromIntegral <$> SDL.ticks
  let wit = allwit frogwork
    in put frogwork { allwit = wit { timewit = keepTime (timewit wit) now } }

listenKeys :: StateT Frogwork IO ()
listenKeys = do
  frogwork <- get
  let wit = allwit frogwork
    in put frogwork { allwit = wit { keyset = bethinkKeys (events wit) (keyset wit) } }

listenMouse :: StateT Frogwork IO ()
listenMouse = do
  frogwork <- get
  -- todo sum instead of head
  let wit = allwit frogwork
      f x = if full x then head x else GL.Vertex2 0 0
      mouse = twimap f (doBoth unwrapHappenPointer unwrapHappenWheel $ events wit)
    in put frogwork { allwit = wit { mouse = uncurry Mousewit mouse } }

listenWindow :: StateT Frogwork IO ()
listenWindow = do
  frogwork <- get
  let wit = allwit frogwork
    in when (or . unwrapHappenWindow $ events wit) $ do
      dis <- lift (waxwane $ window wit)
      teller' <- lift $ execStateT flushWritings (stateteller frogwork)
      put frogwork {
        allwit = wit { display = dis },
        stateteller = teller'
      }

waxwane :: SDL.Window -> IO RenderView
waxwane wind = do
  SDL.V2 width height <- (fromIntegral <$>) <$> SDL.glGetDrawableSize wind
  GL.viewport $= (Position 0 0, Size width height)
  GL.depthFunc $= Just Lequal
  GL.blend $= Enabled
  GL.blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  return RenderView {
    aspect = fromIntegral width / fromIntegral height
  , size = (fromIntegral width, fromIntegral height)
  , fov = pi / 4.0
  , near = 0.1
  , far = 100.0
  }
