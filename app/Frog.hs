module Frog where

import Light

makeFrog :: Polygon
makeFrog = map (fmap (/ 8) . dir) [pi / 2, 7 * pi / 6, 11 * pi / 6]
