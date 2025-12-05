module Weird where

import Control.Monad.State (MonadState (get, put), State, runState)
import System.Random (Random (randoms), mkStdGen)

import Mean
import Time


type FrogSeed = Twain [Float]

formseed :: IO FrogSeed
formseed = ([], ) . randoms . mkStdGen <$> seedtide
  where
    seedtide = tenny . utctDayTime <$> getCurrentTime
    tenny x
      | abs (x - fromIntegral (round x)) < 1e-12 = round x
      | otherwise = tenny (10*x)

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
