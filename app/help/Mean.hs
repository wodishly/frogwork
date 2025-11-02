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

-- | debugging

-- | generalized loudly
ly' :: Show b => (a -> b) -> a -> a
-- ly' f x = trace (show (f x)) x
ly' = (. twin) . (uncurry trace .) . (first . (show .))

-- | loudly
ly :: Show a => a -> a
ly = ly' id

-- | wah
weep :: IO ()
weep = print "wah"

-- | end debugging

-- given that `f thing` holds, return `thing`
-- otherwise, return `bad`
given :: (a -> Bool) -> a -> a -> a
given f thing bad = if f thing then thing else bad

-- assert `isJust x`, and then unpack and return `x`
wis :: Maybe a -> a
wis x = assert (isJust x) (fromJust x)

twin :: a -> (a, a)
twin x = (x, x)

-- given a dyadic function `f`, applies `x` to both arguments
untwin :: (a -> a -> b) -> a -> b
untwin f x = f x x

-- a -> (f a, g a)
applyBoth :: (a -> b) -> (a -> c) -> a -> (b, c)
applyBoth f g = bimap f g . twin

-- applies a function to both arguments of a pair
twimap :: (a -> b) -> (a, a) -> (b, b)
twimap = untwin bimap

-- foldlikes

-- | >>> flight 4
-- [0,1,2,3]
flight :: Shell Int
flight = enumFromTo 0 . pred

-- | Whether @x@ gladdens all @f@ in @fs@.
allIn :: Foldable t => t (a -> Bool) -> a -> Bool
allIn = flip (.) (&) . flip all

-- | Whether @x@ gladdens any @f@ in @fs@.
anyIn :: Foldable t => t (a -> Bool) -> a -> Bool
anyIn fs x = any ($ x) fs

-- | Whether the argument is unempty.
full :: Foldable t => t a -> Bool
full = not.null

-- split xs into a list of lists xss where each `last xss` gladdens `f`
split :: (a -> Bool) -> Shell [a]
split f = split' f . map singleton

-- | Wraps @arg@ in a list.
--
-- >>> shell "frog"
-- ["frog"]
shell :: Shell a
shell = (: [])

-- | Whether the argument has more than one thing.
multipleton :: Foldable t => t a -> Bool
multipleton = (> 1) . length

none :: Foldable t => (a -> Bool) -> t a -> Bool
none = (not .) . any

-- | Returns all but the last @n@ elements of @xs@.
-- A better name is wanting.
--
-- >>> leave 3 [0,1,2,3,4,5,6,7]
-- [0,1,2,3,4]
leave :: Int -> Shift [a]
leave n xs = take (length xs - n) xs

-- | Returns the last @n@ elements of @xs@.
-- A better name is wanting.
--
-- >>> scoop 3 [0,1,2,3,4,5,6,7]
-- [5,6,7]
scoop :: Int -> Shift [a]
scoop n xs = drop (length xs - n) xs

-- | Hits the @n@th thing in @xs@ with @f@.
--
-- >>> hit 2 (*10) [0,1,2,3]
-- [0,1,20,3]
hit :: Int -> Shift a -> Shift [a]
hit n f xs = take n xs ++ [f (xs!!n)] ++ drop (n+1) xs

-- | For each @n@ in @ns@, hits the @n@th thing in @xs@ with @f@.
--
-- >>> hits [1,2] (*10) [0,1,2,3]
-- [0,10,20,3]
hits :: [Int] -> Shift a -> Shift [a]
hits [] _ = id
hits ns f = hits (tail ns) f . hit (head ns) f

-- for when you feel bad about having to choose from between two of the same thing
samely :: Eq a => a -> a -> a
samely l r = assert (l == r) l

split' :: (a -> Bool) -> Shift [[a]]
split' f (a:b:rest) = if (f.last) a
  then a : split' f (b:rest)
  else split' f ((a++b):rest)
split' _ xs = xs

lif :: Bool -> Bool -> Bool
lif = (||) . not
