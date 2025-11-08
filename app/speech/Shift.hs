{- HLINT ignore "Use section" -}
module Shift where

import Data.Bifunctor (bimap)
import Data.Function (applyWhen)
import Data.Maybe (catMaybes)

import Breath (Breath (..), Bright, flatten, makeBright, shiftInset, shiftOnset)
import Loud (Flight, Loud, Loudmark (..), isDerm, isRough, isThroat, offbearThroat, onbearThroat, unbear, unloud)
import Mark (become, get, on, set, worth)

import Mean (Shed, Shift, full, hit, leave, samely, shell)


queue :: Shed (Shift a)
queue = foldr (.) id

-- if `cs`, then do shift `f` on loud `l`
shif :: [Loud -> Bool] -> Shift (Shift Loud)
shif cs f l = if (and.sequence cs) l then f l else l

borrow :: Loudmark -> Loud -> Shift Loud
borrow = (set .) . get

-- blind to prosodic structure !!
-- kill all in `w` that meet `c`
kill :: (Loud -> Bool) -> Shift Bright
kill c = float (filter (not.c))

-- beget `x` before all in `w` that meet `c`
begetl :: Loud -> (Loud -> Bool) -> Shift Bright
begetl = beget id

-- beget `x` after all in `w` that meet `c`
begetr :: Loud -> (Loud -> Bool) -> Shift Bright
begetr = beget reverse

beget :: Shift Flight -> Loud -> (Loud -> Bool) -> Shift Bright
beget f x c = makeBright . beget' f x c . flatten

beget' :: Shift Flight -> Loud -> (Loud -> Bool) -> Shift Flight
beget' f x c = foldr (\l -> (++) (f ([x | c l] ++ [l]))) []

die :: Shift Loud
die = become unloud

sweep :: Shift Flight
sweep = filter (/= unloud)

sweepWord :: Shift Bright
sweepWord = float sweep

-- send a flight to a flight of tuples that encodes awareness of the nearest neighbors

leftly :: Int -> Flight -> [(Flight, Loud)]
leftly 0 ls = map ([], ) ls
leftly n ls = zipWith (\x y -> bimap ((++) (fst x)) (samely (snd x)) y)
  (zipWith (\x y -> (catMaybes x, y))
    (map shell $ replicate n Nothing ++ map Just (leave n ls)) ls)
  (leftly (n-1) ls)

rightly :: Int -> Flight -> [(Loud, Flight)]
rightly 0 ls = map (, []) ls
rightly n ls = zipWith (\x y -> bimap (samely (fst x)) ((++) (snd x)) y)
  (rightly (n-1) ls)
  (zipWith (\x y -> (x, catMaybes y))
    ls (map shell $ map Just (drop n ls) ++ replicate n Nothing))

bothly :: Int -> Int -> Flight -> [(Flight, Loud, Flight)]
bothly m n ls = zipWith (\x y -> (fst x, samely (snd x) (fst y), snd y))
  (leftly m ls) (rightly n ls)

-- deepshift to shoalshift
float :: Shift Flight -> Shift Bright
float f = makeBright.f.flatten

-- shoalshift to deepshift
sink :: Shift Bright -> Shift Flight
sink f = flatten.f.makeBright

workAll :: Shift Loud -> Shift Bright
workAll = float.map

workFirst :: Shift Loud -> Shift Bright
workFirst f bs = hit 0 ((if full.onset.head $ bs then shiftOnset else shiftInset) (hit 0 f)) bs

killFirstIf :: [Loud -> Bool] -> Shift Bright
killFirstIf cs = float sweep . workFirst (shif cs die)

offbear :: Shift Flight
offbear = map (\x -> if isDerm x then (if isThroat x then offbearThroat x else unbear x) else x)

onbear :: Shift Flight
onbear = map onbear' . bothly 1 1

onbear' :: (Flight, Loud, Flight) -> Loud
onbear' (l, m, r) = applyWhen (not (any (any (worth Bear)) [l, r]))
  (if isThroat m
    then onbearThroat
    else applyWhen (worth Smooth m) (on Bear)
  ) m

gainbear :: Shift Bright
gainbear = float (onbear.offbear)

nosesame :: Shift Bright
nosesame = makeBright . map nosesame' . rightly 1 . flatten

nosesame' :: (Loud, Flight) -> Loud
nosesame' (l, [r]) = applyWhen (worth Nose l && isRough r && (not.worth Thru) r) (borrow Mouth r) l
nosesame' (l, []) = l
nosesame' _ = error "bad nosesame"

stavefold :: Shift Bright
stavefold = float stavefold'

stavefold' :: Shift Flight
stavefold' ls = case ls of
-- todo: remember why this line is commented
--  (a:b:rest) -> lif (worth' Bear a && (on Long a == on Long b)) (stavefold' (on Long b:rest)) (a: stavefold' (b:rest))
  (a:b:rest) -> if all (\x -> worth Bear x && not (isDerm x)) [a,b]
    then stavefold' (on Long a:rest)
    else a: stavefold' (b:rest)
  stuff -> stuff

lurk :: [Shift Bright] -> Shift (Shift Bright)
lurk shs f = queue (shs++[f])

-- do `f` until `br == f br`
gainly :: Shift Bright -> Shift Bright
gainly f br = until (== f br) f br