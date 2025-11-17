module State (
  StateName (..)
, Stately (..)
, doAt
, doEvery
) where

import Control.Monad (when)
import Control.Monad.State (StateT)

import Mean (between)
import Time (Timewit (..))
import Allwit (Allwit)
import Data.Fixed (mod')


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

  render :: Allwit -> StateT a IO ()
  render _ = return ()

  loop :: Allwit -> StateT a IO Allwit
  loop allwit = do
    w' <- update allwit
    render w'
    return w'

doAt :: Stately a => Timewit -> Float -> StateT a IO () -> StateT a IO ()
doAt time = when . between (lifetime time, lifetime time + delta time)

doEvery :: Stately a => Timewit -> Float -> StateT a IO () -> StateT a IO ()
doEvery time t = when (mod' (lifetime time) t > mod' (lifetime time + delta time) t)
