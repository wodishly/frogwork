module Weird where

import Control.Monad.State (MonadState (get, put), State, runState)
import System.Random (Random (randoms), mkStdGen)


type FrogSeed = ([Float], [Float])

formseed :: FrogSeed
formseed = ([], randoms (mkStdGen 0))

next :: State FrogSeed Float
next = do
  (left, right) <- get
  put (head right:left, tail right)
  return (head right)

repeatState :: Int -> State s a -> s -> ([a], s)
repeatState 1 f s = ([a], s')
  where (a, s') = runState f s
repeatState n f s = (a:as, t)
  where (a, s') = runState f s
        (as, t) = repeatState (pred n) f s'
