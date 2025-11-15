module Time (
  Timewit (..)
, beginTime
, keepTime
, doCurrentTime
, throttle
) where

import Data.Word (Word32)
import Graphics.Rendering.OpenGL (GLfloat)
import qualified SDL.Time as SDL

data Timewit = Timewit {
  lifetime :: Word32
, delta :: Word32
} deriving (Show, Eq)

doCurrentTime :: IO Float
doCurrentTime = do
  t <- SDL.ticks
  return $ fromIntegral t / 1000

beginTime :: Timewit
beginTime = Timewit 0 0

keepTime :: Timewit -> Word32 -> Timewit
keepTime time now = Timewit {
    lifetime = now
  , delta = now - lifetime time
}

throttle :: Timewit -> GLfloat -> GLfloat
throttle time = (*) (fromIntegral (delta time) / 1000)
