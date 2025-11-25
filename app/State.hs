module State where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.Fixed

import Allwit
import Mean
import Time


data StateName
  = TitleName
  | WillName
  | PlayName
  | PauseName
  | AboutName
  | EndName
  deriving (Show, Eq, Ord)

class Stately a where
  name :: a -> StateName

  update :: Allwit -> StateT a IO Allwit
  update = return

  render :: Allwit -> StateT a IO Allwit
  render = return

  loop :: Allwit -> StateT a IO Allwit
  loop allwit = update allwit >>= render

doOnceAt :: Stately a => Timewit -> Float -> StateT a IO () -> StateT a IO ()
doOnceAt (Timewit { lifetime, delta }) = when . between (second (+delta) (twin lifetime))

doAtEach :: Stately a => Timewit -> Float -> StateT a IO () -> StateT a IO ()
doAtEach (Timewit { lifetime, delta }) t = when (mod' lifetime t > mod' (lifetime + delta) t)
