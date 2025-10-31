{- HLINT ignore "Use infix" -}
module Time where

import Data.Word (Word32)

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

import Mean
import Fast

data Time = Time Word32 [Word32]

instance Show Time where
  show time@(Time t _)
    = (pad 2 . Left . fromMaybe 0 . meanGap) time ++ "fps"
    ++ " (Lifetime: " ++ show t ++ " ms)"

startTime :: Time
startTime = Time 0 []

keepTime :: Time -> Word32 -> Time
keepTime time@(Time _ gaps) now = Time now (take (4*gapGoal) (gap time now:gaps))

gap :: Time -> Word32 -> Word32
gap (Time lifetime _) now = now - lifetime

meanGap :: Time -> Maybe Double
meanGap (Time _ gaps) = if not (null gaps)
  then Just (1000 / average gaps)
  else Nothing

-- pad either a number or string with '0'
pad :: (Show a, Num a) => Int -> Either a String -> String
pad = pad' '0'

-- pad either a number or string with `peanut`
pad' :: (Show a, Num a) => Char -> Int -> Either a String -> String
pad' peanut noughts eithered = pad'' peanut noughts (uneithered++(if elem '.' uneithered then "" else "."))
  where uneithered = case eithered of
          Left rime -> show rime
          Right word -> word

-- pad a string with `peanut`
pad'' :: Char -> Int -> String -> String
pad'' peanut noughts word
  | handed > pure 0 = pad'' peanut noughts (word++[peanut])
  | handed < pure 0 = pad'' peanut noughts (take (length word - 1) word)
  | otherwise = word
  where handed = fmap (noughts -) (elemIndex '.' (reverse word))