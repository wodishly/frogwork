{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module Breath where

import Data.List (findIndex)
import Data.Maybe (maybeToList, fromJust)
import Data.Bifunctor (first)

import Mark
import Token
import Loud
import Mean

type Onset = Flight
type Inset = Flight
type Offset = Flight

data Rime = Rime {
  inset :: Inset,
  offset :: Offset
} deriving (Eq)

instance Show Rime where
  show (Rime i o) = "(" ++ concatMap cleans [i,o] ++ ")"

data Breath = Breath {
  onset :: Onset,
  rime :: Rime,
  sharp :: Bool
} deriving (Eq)

instance Show Breath where
  show (Breath o r s) = "(" ++ cleans o ++ stuff ++ ")"
    where stuff = if s
                  then "(" ++ fromJust (lookup (clean $ head (inset r)) sharps)
                           ++ concatMap cleans [tail (inset r), offset r] ++ ")"
                  else show r

-- breath + flight
-- todo: a better name
type Bright = [Breath]

unrime :: Rime
unrime = Rime [] []

shiftOnset :: Shift Flight -> Shift Breath
shiftOnset sh b = Breath (sh $ onset b) (rime b) (sharp b)

shiftRime :: Shift Flight -> Shift Flight -> Shift Breath
shiftRime sh1 sh2 b = Breath (onset b) (Rime (sh1 $ inset $ rime b) (sh2 $ offset $ rime b)) (sharp b)

shiftInset :: Shift Flight -> Shift Breath
shiftInset = flip shiftRime id

shiftOffset :: Shift Flight -> Shift Breath
shiftOffset = shiftRime id

hasOnset :: Breath -> Bool
hasOnset = full.onset

hasInset :: Breath -> Bool
hasInset = full.inset.rime

hasOffset :: Breath -> Bool
hasOffset = full.offset.rime

hasRime :: Breath -> Bool
hasRime b = hasInset b || hasOffset b

isHeavy :: Breath -> Bool
isHeavy = hasOffset

isLight :: Breath -> Bool
isLight = not.isHeavy

makeRime :: Flight -> Rime
makeRime ls = case findIndex (not.worth Bear) ls of
  Just n -> uncurry Rime (splitAt n ls)
  Nothing -> Rime ls []

makeBreath :: Flight -> Breath
makeBreath ls = case findIndex (worth Bear) ls of
  Just n -> (\x -> Breath (fst x) (makeRime $ snd x) False) (splitAt n ls)
  Nothing -> Breath ls unrime False

-- todo: can probably use functors to beautify these guys
thrifork :: Breath -> [Flight]
thrifork b = filter full (map ($ b) thrifork')

thrifork' :: [Breath -> Flight]
thrifork' = [onset, inset.rime, offset.rime]

-- flatten prosodic structure of word into flight of loudness
flatten :: Bright -> Flight
flatten (b:bs) = concat (thrifork b++[flatten bs])
flatten _ = []

-- nudge the breathbreak so that the bad onset becomes a good offset
nudge :: Shift Bright
nudge (a:b:rest)
  = (\(o, b') -> shiftOffset (++o) a : nudge (maybeToList b'++rest))
    (trimBadOnset b)
nudge w = w

trimBadOnset :: Breath -> (Onset, Maybe Breath)
trimBadOnset b
  | (not.hasRime) b = (onset b, Nothing)
  | (isGoodOnset.onset) b = ([], Just b)
  | otherwise = first ([(head.onset) b] ++) (trimBadOnset $ shiftOnset tail b)

-- todo: an actual version of this
isGoodOnset :: Onset -> Bool
isGoodOnset o = lif (multipleton o) (elem o (map dirtys ["t", "tp"]))

makeBright :: Flight -> Bright
makeBright = nudge . map makeBreath . split (worth Bear)

-- in loudness
lengthL :: Bright -> Int
lengthL = length' id

-- in tokens
lengthT :: Bright -> Int
lengthT = length' cleans

length' :: Foldable f => (Flight -> f a) -> Bright -> Int
length' f bs
  = sum (map (\b -> (sum . map (length . (\g -> (f.g) b))) thrifork') bs)