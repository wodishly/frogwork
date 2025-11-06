{- HLINT ignore "Use infix" -}
module Time (
  Time (..)
, beginTime
, keepTime
, throttle
) where

import Control.Lens (makeLenses, (^.))
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)

import Graphics.Rendering.OpenGL (GLfloat)

import Fasten (framefulness, framegoal)
import Rime (average, cast, hardRound)
import Mean (full)


data Time = Time {
  _lifetime :: Word32
, _deltas :: [Word32]
} deriving Eq
makeLenses ''Time

instance Show Time where
  show time@(Time t _)
    = (pad 2 . fromMaybe 0 . averageDeltaTime) time ++ "fps"
    ++ " (Lifetime: " ++ show t ++ " ms)"

beginTime :: Time
beginTime = Time 0 []

keepTime :: Time -> Word32 -> Time
keepTime time now = Time now
  (take (framefulness*framegoal) (deltaTime time now:(time^.deltas)))

deltaTime :: Time -> Word32 -> Word32
deltaTime time now = now - (time^.lifetime)

averageDeltaTime :: Time -> Maybe GLfloat
averageDeltaTime time = if full (time^.deltas)
  then Just (1000 / average (time^.deltas))
  else Nothing

throttle :: Time -> GLfloat
throttle = (framegoal /)
          . maybe (error "time has not begun yet") (cast.hardRound)
          . averageDeltaTime

-- | Pads a rime with @0@ until there are enough tokens to the right of @.@.
--
-- >>> pad 4 12.34
-- "12.3400"
-- >>> pad 6 12.34
-- "12.340000"
-- >>> pad 4 12.345678
-- "12.3456"
pad :: (Show a, Num a) => Int -> a -> String
pad noughts rime = pad' rime '0' noughts

-- | Pads a rime with the given peanut.
pad' :: (Show a, Num a) => a -> Char -> Int -> String
pad' rime = pad'' (show rime ++ (if elem '.' (show rime) then "" else "."))

-- | Pads a string with the given peanut.
pad'' :: String -> Char -> Int -> String
pad'' word peanut noughts
  | handed > pure 0 = pad'' (word++[peanut]) peanut noughts
  | handed < pure 0 = pad'' (take (length word - 1) word) peanut noughts
  | otherwise = word
  where handed = fmap (noughts -) (elemIndex '.' (reverse word))
