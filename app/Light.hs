module Light where

import Data.Ord
import Data.Word
import Foreign.C
import Mean
import SDL

white :: V4 Word8
white = V4 255 255 255 255

black :: V4 Word8
black = V4 0 0 0 0

green :: V4 Word8
green = V4 0 255 0 255

blue :: V4 Word8
blue = V4 0 0 255 255

-- interpolates a shade of green
green' :: CFloat -> V4 Word8
green' = clerp green

-- interpolates a color `c` by fraction `n`
clerp :: V4 Word8 -> CFloat -> V4 Word8
clerp c n = fmap (cast . (*) (clamp (0, 1) n) . cast) c
