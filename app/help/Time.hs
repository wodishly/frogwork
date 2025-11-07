module Time (
  Time (..)
, beginTime
, keepTime
, lifetime
, delta
) where

import Control.Lens (makeLenses, (^.))
import Data.Word (Word32)


data Time = Time {
  _lifetime :: Word32
, _delta :: Word32
} deriving (Show, Eq)
makeLenses ''Time

beginTime :: Time
beginTime = Time 0 0

keepTime :: Time -> Word32 -> Time
keepTime time now = Time {
    _lifetime = now
  , _delta = now - (time^.lifetime)
}
