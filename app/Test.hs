module Test where
import Random
import Control.Monad
import Mean

runTests :: IO ()
runTests = do
  mapM_ (replicateM 1000000 >=> print.average) [rand, rx, ry, rz, rangle]
  (replicateM 1000000 >=> print.average) rdir
  return ()