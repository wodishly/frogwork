module Test where
import Control.Monad

import Random
import Rime

someFand :: IO ()
someFand = mapM_ (replicateM 1000000 >=> print.average) [rand, rx, ry, rangle]