{-# LANGUAGE TemplateHaskell #-}
{- HLINT ignore "Use infix" -}

module Time where

import Data.Word (Word32)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Control.Lens

import Graphics.Rendering.OpenGL (GLfloat)

import Rime
import Fast

data Time = Time {
  _lifetime :: Word32
, _deltas :: [Word32]
}
makeLenses ''Time

instance Show Time where
  show time@(Time t _)
    = (pad 2 . fromMaybe 0 . averageDeltaTime) time ++ "fps"
    ++ " (Lifetime: " ++ show t ++ " ms)"

beginTime :: Time
beginTime = Time 0 [17]

keepTime :: Time -> Word32 -> Time
keepTime time now = Time now (take (framefulness*framegoal :: Int) (deltaTime time now:(time^.deltas)))

deltaTime :: Time -> Word32 -> Word32
deltaTime time now = now - (time^.lifetime)

averageDeltaTime :: Time -> Maybe GLfloat
averageDeltaTime time = if not (null (time^.deltas))
  then Just (1000 / average (time^.deltas))
  else Nothing

throttle :: Time -> GLfloat
throttle time = framegoal / maybe (error "time has not begun yet") (cast.hardRound) (averageDeltaTime time)

ms :: Num a => a -> a
ms = (* 1000)

-- pad a number with '0'
pad :: (Show a, Num a) => Int -> a -> String
pad = pad' '0'

-- pad a number with `peanut`
pad' :: (Show a, Num a) => Char -> Int -> a -> String
pad' peanut noughts rime = pad'' peanut noughts (show rime ++(if elem '.' (show rime) then "" else "."))

-- pad a string with `peanut`
pad'' :: Char -> Int -> String -> String
pad'' peanut noughts word
  | handed > pure 0 = pad'' peanut noughts (word++[peanut])
  | handed < pure 0 = pad'' peanut noughts (take (length word - 1) word)
  | otherwise = word
  where handed = fmap (noughts -) (elemIndex '.' (reverse word))