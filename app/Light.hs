module Light where

import SDL
import Data.Ord
import Data.Word
import Foreign.C

import Mean

type Color = V4 Word8

white :: Color
white = V4 255 255 255 255

black :: Color
black = V4 0 0 0 0

red :: Color
red = V4 255 0 0 255

green :: Color
green = V4 0 255 0 255

blue :: Color
blue = V4 0 0 255 255

-- interpolates a color `c` by fraction `n`
clerp :: CFloat -> Color -> Color
clerp n = fmap (cast . (*) (clamp (0, 1) n) . cast)