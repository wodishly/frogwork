module EndState (
  EndState (..)
, makeEndState
) where

import State (StateName (EndName), Stately (..))

import Stavemake (Staveware)


newtype EndState = EndState Staveware

instance Stately EndState where
  name _ = EndName
  staveware (EndState ware) = ware

  update _ = return ()
  render _ = return ()

makeEndState :: Staveware -> EndState
makeEndState = EndState
