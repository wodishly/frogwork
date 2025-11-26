{- HLINT ignore "Use section" -}
module Tung.Mark (
  Branch,
  Mark (..),
  on,
  ons,
  off,
  offs,
  worth,
  worths,
  become,
  get,
  set,
) where

import Control.Monad (join)
import Data.Foldable (find)
import Data.Function (applyWhen)
import Data.Maybe (fromJust, isJust)

import Mean (Shift)


class (Show a, Eq a) => Mark a where
  isAxled, isSteadfast :: a -> Bool
  below' :: a -> [a]
  below, above :: a -> a -> Bool
  below m m' = or $ sequence [elem m', any (flip below m')] (below' m)
  above = flip below

  def :: a -> Branch a
  def m = Branch m True (map (off' . def) (below' m))

data Branch a = Branch {
  _mark :: a,
  _worth :: Bool,
  _children :: [Branch a]
} deriving (Eq)

instance Mark a => Show (Branch a) where
  show = init . showMark 0

newtype Withmete a = Withmete (Bool, Bool, a)

instance Mark a => Show (Withmete a) where
  show (Withmete (l, r, m)) = "[" ++ showWorth m l ++ "/" ++ showWorth m r ++ show m ++ "]"

showWorth :: Mark a => a -> Bool -> String
showWorth m b
  | b = "+"
  | isAxled m = "-"
  | otherwise = "0"

showMark :: Mark a => Int -> Branch a -> String
showMark n (Branch m w cs)
  = concat (replicate n " ") ++ "[" ++ showWorth m w ++ show m ++ "]"
 ++ "\n" ++ (\x -> if not $ null x then x else "") (concatMap (showMark $ n+1) cs)

-- leaf :: Branch a -> Bool
-- leaf = null._children

worth :: Mark a => a -> Branch a -> Bool
worth m' (Branch m w cs) = m==m' && w || any (worth m') cs

worths :: Mark a => [a] -> Branch a -> Bool
worths ms l = all (flip worth l) ms

become :: Mark a => Branch a -> Shift (Branch a)
become = const

-- on' :: Mark a => Shift (Branch a)
-- on' l = on (_mark l) l

off' :: Mark a => Shift (Branch a)
off' l = off (_mark l) l

-- un' :: Mark a => Shift (Branch a)
-- un' l = un (_mark l) l

ons :: Mark a => [a] -> Shift (Branch a)
ons = (flip . foldr) on

offs :: Mark a => [a] -> Shift (Branch a)
offs = (flip . foldr) off

get :: Mark a => a -> Shift (Branch a)
get = (fromJust .) . get'

set :: Mark a => Branch a -> Shift (Branch a) --Loud -> Shift Loud
set b l = if _mark b == _mark l
  then b
  else fandUp $ Branch (_mark l) (_worth l) (map (set b) (_children l))

fandUp :: Mark a => Shift (Branch a)
fandUp (Branch m w cs) = (\x -> Branch m (not (isSteadfast m || not (any _worth x)) || w) x)
                         (map fandUp cs)

on :: Mark a => a -> Shift (Branch a)
on n (Branch m w cs) = if m==n
  then Branch m True cs
  else fandUp (Branch m w $ map (on n) cs)

off :: Mark a => a -> Shift (Branch a)
off n (Branch m w cs) = if m==n
  then Branch m False (applyWhen (not (isSteadfast m)) (map off') cs)
  else Branch m w (map (off n) cs)

-- un :: Mark a => a -> Shift (Branch a)
-- un n (Branch m w cs) = if m==n
--   then (if w then off' else on') (Branch m w cs)
--   else Branch m w (map (un n) cs)

get' :: Mark a => a -> Branch a -> Maybe (Branch a)
get' m l = if m == _mark l
  then Just l
  else join $ find isJust $ map (get' m) (_children l)

-- withmete :: Mark a => Branch a -> Branch a -> [Withmete a]
-- withmete left right = [
--     Withmete (_worth (get (_mark right) left), _worth right, _mark right)
--     | _worth right /= _worth (get (_mark right) left)
--   ] ++ concatMap (withmete left) (_children right)