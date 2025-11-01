module Test where
import Random
import Control.Monad
import Rime

someFand :: IO ()
someFand = mapM_ (replicateM 1000000 >=> print.average) [rand, rx, ry, rangle]