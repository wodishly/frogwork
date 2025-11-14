module Time (
  Time (..)
, beginTime
, keepTime
, doCurrentTime
, throttle
) where

import Data.Word (Word32)
import Graphics.Rendering.OpenGL (GLfloat)
import qualified SDL.Time as SDL

data Time = Time {
  lifetime :: Word32
, delta :: Word32
} deriving (Show, Eq)

doCurrentTime :: IO Float
doCurrentTime = do
  t <- SDL.ticks
  return $ fromIntegral t / 1000

beginTime :: Time
beginTime = Time 0 0

keepTime :: Time -> Word32 -> Time
keepTime time now = Time {
    lifetime = now
  , delta = now - lifetime time
}

throttle :: Time -> GLfloat -> GLfloat
throttle time = (*) (fromIntegral (delta time) / 1000)
