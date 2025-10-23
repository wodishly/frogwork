module Light where

import SDL
import Data.Ord
import Data.Word
import Foreign.C

import Mean

white :: V4 Word8
white = V4 255 255 255 255

black :: V4 Word8
black = V4 0 0 0 0

green :: V4 Word8
green = V4 0 255 0 255

green' :: CFloat -> V4 Word8
green' = hue green

hue :: V4 Word8 -> CFloat -> V4 Word8
hue c n = fmap (cast . (*) (clamp (0, 1) n) . cast) c
