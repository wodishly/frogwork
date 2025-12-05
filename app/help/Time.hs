module Time (
  module Time,
  module Data.Time,
) where

import Data.Time (getCurrentTime, UTCTime (utctDayTime))


data Timewit = Timewit {
  delta :: Float,
  lifetime :: Float
} deriving (Show, Eq)

beginTime :: Float -> Timewit
beginTime = Timewit 0

keepTime :: Timewit -> Float -> Timewit
keepTime Timewit { lifetime } ticks =
  let t = ticks / 1000
  in Timewit {
    lifetime = t,
    delta = t - lifetime
  }

throttle :: Timewit -> Float -> Float
throttle Timewit { delta } = (delta *)
