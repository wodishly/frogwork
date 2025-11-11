module Time (
  Time (..)
, beginTime
, keepTime
, throttle
) where

import Data.Word (Word32)
import Graphics.Rendering.OpenGL (GLfloat)


data Time = Time {
  lifetime :: Word32
, delta :: Word32
} deriving (Show, Eq)

beginTime :: Time
beginTime = Time 0 0

keepTime :: Time -> Word32 -> Time
keepTime time now = Time {
    lifetime = now
  , delta = now - lifetime time
}

throttle :: Time -> GLfloat -> GLfloat
throttle time = (*) (fromIntegral (delta time) / 1000)
