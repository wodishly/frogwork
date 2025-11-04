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
, _gaps :: [Word32]
}
makeLenses ''Time

instance Show Time where
  show time@(Time t _)
    = (pad 2 . fromMaybe 0 . meanGap) time ++ "fps"
    ++ " (Lifetime: " ++ show t ++ " ms)"

beginTime :: Time
beginTime = Time 0 [17]

keepTime :: Time -> Word32 -> Time
keepTime time now = Time now (take (framefulness*framegoal :: Int) (latestGap time now:(time^.gaps)))

latestGap :: Time -> Word32 -> Word32
latestGap time now = now - (time^.lifetime)

meanGap :: Time -> Maybe GLfloat
meanGap time = if not (null (time^.gaps))
  then Just (1000 / average (time^.gaps))
  else Nothing

throttle :: Time -> GLfloat
throttle time = framegoal / maybe (error "time has not begun yet") (cast.hardRound) (meanGap time)

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