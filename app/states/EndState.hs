module EndState where

import State


data EndState = EndState

instance Stately EndState where
  name _ = EndName

makeEndState :: EndState
makeEndState = EndState
