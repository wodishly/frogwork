module Spell (
  f32x3
, summon
, int
, (✿)
) where

import Control.Monad (replicateM)
import Data.Binary.Get (Get, getFloatle)
import Graphics.Rendering.OpenGL (GLfloat, Vertex3 (Vertex3))
import qualified Data.ByteString.Lazy as BL

f32x3 :: Get (Vertex3 GLfloat)
f32x3 = do
  x <- getFloatle
  y <- getFloatle
  z <- getFloatle
  return $! Vertex3 x y z

summon :: String -> IO BL.ByteString
summon = BL.readFile

(✿) :: Applicative m => Integral i => i -> m a -> m [a]
(✿) x = replicateM (fromIntegral x)

int :: Integral i => i -> Integer
int = fromIntegral
