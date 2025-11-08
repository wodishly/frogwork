module Mean where

import Debug.Trace (trace)
import Data.Bifunctor (first, bimap)
import Control.Exception (assert)
import Data.List (singleton)
import Data.Function (applyWhen)

type Shed a = [a] -> a
type Shell a = a -> [a]
type Shift a = a -> a
type Twain a = (a, a)

-- @region For working with bugs.

-- | generalized loudly
ly' :: Show b => (a -> b) -> a -> a
-- ly' f x = trace (show (f x)) x
ly' = (. twin) . (uncurry trace .) . (first . (show .))

-- | loudly
ly :: Show a => a -> a
ly = ly' id

-- | generalized loudly on "wah"
sadly :: a -> a
sadly = ly' (const ("wah" :: String))

-- | wah
weep :: IO ()
weep = print ("wah" :: String)

-- @endregion

-- @region For working with failables.

-- | Asserts that @l == r@, and then returns the value.
--
-- >>> samely "frog" "frog"
-- "frog"
-- >>> samely "frog" "toad"
-- Assertion failed
samely :: Eq a => a -> a -> a
samely l r = assert (l == r) l

-- @endregion

-- @region For working with twains.

-- | Given that @f thing@ holds, return @thing@.
-- Otherwise, return the argument.
{-# INLINE given #-}
given :: (a -> Bool) -> a -> a -> a
given f thing = applyWhen (f thing) (const thing)

-- | Lifts the argument into a twain.
--
-- >>> twin "frog"
-- ("frog","frog")
{-# INLINE twin #-}
twin :: a -> (a, a)
twin x = (x, x)

-- | Applies the same thing to both arguments of a binary operation.
--
-- A better name is wanting.
--
-- A better description is wanting.
--
-- >>> toBoth (+) 1
-- 2
{-# INLINE toBoth #-}
toBoth :: (a -> a -> b) -> a -> b
toBoth f x = f x x

-- | Returns a twain of both functions applied to the argument.
--
-- >>> applyBoth succ pred 0
-- (1,-1)
{-# INLINE doBoth #-}
doBoth :: (a -> b) -> (a -> c) -> a -> (b, c)
doBoth f g = bimap f g . twin

-- | Applies a function to both things of a twain.
--
-- >>> twimap (*2) (1,2)
-- (2,4)
{-# INLINE twimap #-}
twimap :: (a -> b) -> (a, a) -> (b, b)
twimap = toBoth bimap

-- @endregion

-- @region For working with higher tuples.

{-# INLINE uncurry3 #-}
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

-- @endregion

-- @region For working with lists.

-- | >>> flight 4
-- [0,1,2,3]
{-# INLINE flight #-}
flight :: Shell Int
flight = enumFromTo 0 . pred

-- | Whether the thing withstands all of the ordeals.
{-# INLINE allIn #-}
allIn :: Foldable t => t (a -> Bool) -> a -> Bool
allIn fs x = all ($ x) fs

-- | Whether the thing withstands any of the ordeals.
{-# INLINE anyIn #-}
anyIn :: Foldable t => t (a -> Bool) -> a -> Bool
anyIn fs x = any ($ x) fs

-- | Whether the argument is unempty.
{-# INLINE full #-}
full :: Foldable t => t a -> Bool
full = not.null

-- split xs into a list of lists xss where each `last xss` gladdens `f`
{-# INLINE split #-}
split :: (a -> Bool) -> Shell [a]
split f = split' f . map singleton

split' :: (a -> Bool) -> Shift [[a]]
split' f (a:b:rest) = if (f.last) a
  then a : split' f (b:rest)
  else split' f ((a++b):rest)
split' _ xs = xs

-- | Wraps the argument in a list.
--
-- Alias for 'Data.List.singleton'.
--
-- >>> shell "frog"
-- ["frog"]
{-# INLINE shell #-}
shell :: Shell a
shell = singleton

-- | Whether the argument has more than one thing.
--
-- A better name is wanting.
--
-- >>> multipleton [0,1]
-- True
-- >>> multipleton [0..]
-- True
-- >>> multipleton [0]
-- False
-- >>> multipleton []
-- False
{-# INLINE multipleton #-}
multipleton :: Foldable t => t a -> Bool
multipleton = (> 1) . length

-- | Returns @True@ iff nothing in the list withstands the ordeal.
--
-- By De Morgan's laws, @none f xs@ iff @not (any (not f) xs)@.
{-# INLINE none #-}
none :: Foldable t => (a -> Bool) -> t a -> Bool
none = (not .) . any

-- | Returns all but the last @n@ elements of @xs@.
-- A better name is wanting.
--
-- >>> leave 3 [0,1,2,3,4,5,6,7]
-- [0,1,2,3,4]
{-# INLINE leave #-}
leave :: Int -> Shift [a]
leave n xs = take (length xs - n) xs

-- | Returns the last @n@ elements of @xs@.
-- A better name is wanting.
--
-- >>> scoop 3 [0,1,2,3,4,5,6,7]
-- [5,6,7]
{-# INLINE scoop #-}
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

-- | Logical if.
--
-- >>> lif True True
-- True
-- >>> lif True False
-- False
-- >>> lif False True
-- True
-- >>> lif False False
-- True
{-# INLINE lif #-}
lif :: Bool -> Bool -> Bool
lif = (||) . not
