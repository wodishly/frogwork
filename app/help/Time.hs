module Time where


data Timewit = Timewit {
  delta :: Float,
  lifetime :: Float
} deriving (Show, Eq)

beginTime :: Float -> Timewit
beginTime = Timewit 0

keepTime :: Timewit -> Float -> Timewit
keepTime Timewit { lifetime } ticks =
  Timewit {
    lifetime = now,
    delta = now - lifetime
  } where now = ticks / 1000

throttle :: Timewit -> Float -> Float
throttle Timewit { delta } = (delta *)
