module Time (
  Timewit (..)
, beginTime
, keepTime
, throttle
) where

import Data.Word (Word32)
import Graphics.Rendering.OpenGL (GLfloat)


data Timewit = Timewit {
  lifetime :: Word32
, delta :: Word32
} deriving (Show, Eq)

beginTime :: Timewit
beginTime = Timewit 0 0

keepTime :: Timewit -> Word32 -> Timewit
keepTime time now = Timewit {
    lifetime = now
  , delta = now - lifetime time
}

throttle :: Timewit -> GLfloat -> GLfloat
throttle time = (*) (fromIntegral (delta time) / 1000)
