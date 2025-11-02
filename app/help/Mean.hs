module Mean where

import Debug.Trace (trace)
import Data.Function ((&))
import Data.Bifunctor (first, bimap)
import Control.Exception (assert)
import Data.Maybe (fromJust, isJust)
import Data.List (singleton)

type Shed a = [a] -> a
type Shell a = a -> [a]
type Shift a = a -> a

-- unpack and return a contentful maybe
wis :: Maybe a -> a
wis x = assert (isJust x) (fromJust x)

given :: (a -> Bool) -> a -> a -> a
given f thing bad = if f thing then thing else bad

ly' :: Show b => (a -> b) -> a -> a
-- ly' f x = trace (show (f x)) x
ly' = (. twin) . (uncurry trace .) . (first . (show .))

ly :: Show a => a -> a
ly = ly' id

weep :: IO ()
weep = print "wah"

applyBoth :: (a -> b) -> (a -> c) -> a -> (b, c)
applyBoth f g = bimap f g . twin

twin :: a -> (a, a)
twin x = (x, x)

-- given a dyadic function `f`, applies `x` to both arguments
untwin :: (a -> a -> b) -> a -> b
untwin f x = f x x

-- applies a function to both arguments of a pair
twimap :: (a -> b) -> (a, a) -> (b, b)
twimap = untwin bimap

-- whether `x` gladdens all in `fs`
allIn :: (Foldable t) => t (a -> Bool) -> a -> Bool
allIn = flip (.) (&) . flip all

-- whether `x` gladdens any in `fs`
anyIn :: (Foldable t) => t (a -> Bool) -> a -> Bool
anyIn fs x = any ($ x) fs

flight :: Shell Int
flight = enumFromTo 0 . pred

full :: [a] -> Bool
full = not.null

-- split xs into a list of lists xss where each `last xss` gladdens `f`
split :: (a -> Bool) -> Shell [a]
split f = split' f . map singleton

shell :: Shell a
shell = (: [])

-- for when you feel bad about having to choose from between two of the same thing
samely :: Eq a => a -> a -> a
samely l r = assert (l == r) l

split' :: (a -> Bool) -> Shift [[a]]
split' f (a:b:rest) = if (f.last) a
  then a : split' f (b:rest)
  else split' f ((a++b):rest)
split' _ xs = xs

multipleton :: [a] -> Bool
multipleton = (> 1) . length

lif :: Bool -> Bool -> Bool
lif = (||) . not

none :: Foldable t => (a -> Bool) -> t a -> Bool
none = (not .) . any

-- return all but the last n elements
leave :: Int -> Shift [a]
leave n xs = take (length xs - n) xs

-- return the last n elements
scoop :: Int -> Shift [a]
scoop n xs = drop (length xs - n) xs

-- hit only the nth element
hit :: Int -> Shift a -> Shift [a]
hit n f xs = take n xs ++ [f (xs!!n)] ++ drop (n+1) xs

-- todo: tests
hits :: [Int] -> Shift a -> Shift [a]
hits [] _ = id
hits ns f = hits (tail ns) f . hit (head ns) f
