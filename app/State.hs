module State where

import Allwit
import Mean
import Rime
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

doOnceAt :: Stately a => Float -> Timewit -> StateT a IO () -> StateT a IO ()
doOnceAt t Timewit { lifetime, delta } = when (between (lifetime, lifetime+delta) t)

doAtEach :: Stately a => Float -> Timewit -> StateT a IO () -> StateT a IO ()
doAtEach t Timewit { lifetime, delta } = when (mod' lifetime t > mod' (lifetime + delta) t)
