module State (
  StateName (..)
, Stately (..)
, doAt
) where

import Control.Monad (when)
import Control.Monad.State (StateT)

import Mean (between)
import Time (Timewit (..))
import Allwit (Allwit)


data StateName
  = TitleName
  | WillName
  | PlayName
  | PauseName
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
doAt time t = when $ between (lifetime time, lifetime time + delta time) t
