module Frog where

import SDL
import Foreign.C

data Frog = Frog {
  size :: V3 CFloat
, wealth :: Int
}

frog :: Frog
frog = Frog 64 0