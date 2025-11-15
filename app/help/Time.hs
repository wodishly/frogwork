module Time (
  Timewit (..)
, beginTime
, keepTime
, throttle
) where


data Timewit = Timewit {
  lifetime :: Float
, delta :: Float
} deriving (Show, Eq)

beginTime :: Float -> Timewit
beginTime ticks = Timewit ticks 0

keepTime :: Timewit -> Float -> Timewit
keepTime time ticks = Timewit {
    lifetime = now
  , delta = now - lifetime time
} where now = ticks / 1000

throttle :: Timewit -> Float -> Float
throttle time = (delta time *)
