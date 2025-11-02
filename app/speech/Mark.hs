{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use section" #-}

module Mark where
import Mean
import Data.Maybe (isJust)
import Control.Monad (join)
import Data.Function (applyWhen)
import Data.Foldable (find)

class (Show a, Eq a) => Mark a where
  isAxled, isSteadfast :: a -> Bool
  below' :: a -> [a]
  below, above :: a -> a -> Bool
  below m m' = or $ sequence [elem m', any (flip below m')] (below' m)
  above = flip below
  def :: a -> Branch a
  def m = Branch m True (map (off'.def) (below' m))

data Branch a = Branch {
  mark :: a,
  worth :: Bool,
  children :: [Branch a]
} deriving (Eq)

-- todo
-- instance Functor Branch where

instance Mark a => Show (Branch a) where
  show = init . showMark 0

showWorth :: Mark a => a -> Bool -> String
showWorth m b
  | b = "+"
  | isAxled m = "-"
  | otherwise = "0"

showMark :: Mark a => Int -> Branch a -> String
showMark n (Branch m w cs)
  = concat (replicate n " ") ++ "[" ++ showWorth m w ++ show m ++ "]"
 ++ "\n" ++ (\x -> if not $ null x then x else "") (concatMap (showMark $ n+1) cs)

leaf :: Branch a -> Bool
leaf = null.children

-- (worth b) = b.worth
worth' :: Mark a => a -> Branch a -> Bool
worth' m' (Branch m w cs) = m==m' && w || any (worth' m') cs

worths :: Mark a => [a] -> Branch a -> Bool
worths ms l = all (flip worth' l) ms

become :: Mark a => Branch a -> Shift (Branch a)
become = const

on' :: Mark a => Shift (Branch a)
on' l = on (mark l) l

off' :: Mark a => Shift (Branch a)
off' l = off (mark l) l

un' :: Mark a => Shift (Branch a)
un' l = un (mark l) l

ons :: Mark a => [a] -> Shift (Branch a)
ons = (flip.foldr) on

offs :: Mark a => [a] -> Shift (Branch a)
offs = (flip.foldr) off

get :: Mark a => a -> Shift (Branch a)
get = (wis .) . get'

set :: Mark a => Branch a -> Shift (Branch a) --Loud -> Shift Loud
set b l = if mark b == mark l
  then b
  else fandUp $ Branch (mark l) (worth l) (map (set b) (children l))

fandUp :: Mark a => Shift (Branch a)
fandUp (Branch m w cs) = (\x -> Branch m (not (isSteadfast m || not (any worth x)) || w) x)
                         (map fandUp cs)

on :: Mark a => a -> Shift (Branch a)
on n (Branch m w cs) = if m==n
  then Branch m True cs
  else fandUp (Branch m w $ map (on n) cs)

off :: Mark a => a -> Shift (Branch a)
off n (Branch m w cs) = if m==n
  then Branch m False (applyWhen (not (isSteadfast m)) (map off') cs)
  else Branch m w (map (off n) cs)

un :: Mark a => a -> Shift (Branch a)
un n (Branch m w cs) = if m==n
  then (if w then off' else on') (Branch m w cs)
  else Branch m w (map (un n) cs)

get' :: Mark a => a -> Branch a -> Maybe (Branch a)
get' m l = if m == mark l
  then Just l
  else join $ find isJust $ map (get' m) (children l)

newtype Withmete a = Withmete (Bool, Bool, a)

instance Mark a => Show (Withmete a) where
  show (Withmete (l, r, m)) = "[" ++ showWorth m l ++ "/" ++ showWorth m r ++ show m ++ "]"

withmete :: Mark a => Branch a -> Branch a -> [Withmete a]
withmete left right = [
    Withmete (worth (get (mark right) left), worth right, mark right)
    | worth right /= worth (get (mark right) left)
  ] ++ concatMap (withmete left) (children right)