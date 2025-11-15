module EndState (
  EndState (..)
, makeEndState
) where

import State (StateName (EndName), Stately (..))


data EndState = EndState

instance Stately EndState where
  name _ = EndName

makeEndState :: EndState
makeEndState = EndState
