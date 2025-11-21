module Time (
  Timewit (..),
  beginTime,
  keepTime,
  throttle
) where


data Timewit = Timewit {
  lifetime :: Float,
  delta :: Float
} deriving (Show, Eq)

beginTime :: Float -> Timewit
beginTime = flip Timewit 0

keepTime :: Timewit -> Float -> Timewit
keepTime Timewit { lifetime } ticks = Timewit {
    lifetime = now
  , delta = now - lifetime
} where now = ticks / 1000

throttle :: Timewit -> Float -> Float
throttle Timewit { delta } = (delta *)
