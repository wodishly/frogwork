module Mean where

import Control.Exception (assert)
import Control.Monad.State (MonadTrans (lift), StateT)
import Data.Bifunctor (bimap)
import Data.Bits (Bits (shiftL, shiftR))
import Data.Function (applyWhen, (&))
import Data.List (singleton)
import Debug.Trace (traceShowId, traceShowWith)
import GHC.Stack (HasCallStack)
import Control.Arrow (Arrow(first, second))
import Data.Bool (bool)


type Shed a = [a] -> a
type Shell a = a -> [a]
type Shift a = a -> a
type Twain a = (a, a)

-- @region For working with bugs.

-- | generalized loudly
ly' :: Show b => (a -> b) -> a -> a
ly' = traceShowWith

-- | loudly
ly :: Show a => a -> a
ly = traceShowId

softly :: Show a => (a -> a) -> a -> a
softly _ = id

-- | generalized loudly on "wah"
sadly :: a -> a
sadly = ly' (const "wah")

-- | wah
weep :: IO ()
weep = print "wah"

-- | Curse this not with `(Stately b) =>`, lest @preent@ no longer become @Allwit@.
preent :: Show a => a -> StateT b IO ()
preent = lift . print

-- @endregion

-- @region For working with failables.

-- | Asserts that @l == r@, and then returns the value.
--
-- >>> samely "frog" "frog"
-- "frog"
-- >>> samely "frog" "toad"
-- Assertion failed
samely :: Eq a => a -> a -> a
samely = ($ id) .  sSs . (assert .) . (==)

-- @endregion

-- @region For working with twains.

-- | Given a function and two arguments, return the former argument
-- if the function holds of it; otherwise, return the latter argument
-- (even if the function does not hold of it).
--
-- >>> given (>2) 3 1
-- 3
-- >>> given (>2) 0 4
-- 4
-- >>> given (>2) 0 1
-- 1
{-# INLINE given #-}
given :: (a -> Bool) -> a -> a -> a
given = ($ const) . (sSs . (applyWhen .))

-- | Lifts the argument into a twain.
--
-- >>> twin "frog"
-- ("frog","frog")
{-# INLINE twin #-}
twin :: a -> (a, a)
twin = wWw (,)

-- | Applies the same thing to both arguments of a binary operation.
--
-- An alias for the W combinator @wWw@.
--
-- A better name is wanting.
--
-- >>> toBoth (+) 1
-- 2
{-# INLINE toBoth #-}
toBoth :: (a -> a -> b) -> a -> b
toBoth = wWw

-- | W combinator, so named as not to clutter the namespace.
{-# INLINE wWw #-}
wWw :: (a -> a -> b) -> a -> b
wWw f x = f x x

-- | S combinator, so named as not to clutter the namespace.
--
-- prop> @sSs (f . h) g x = uncurry f (doBoth h g x)@
{-# INLINE sSs #-}
sSs :: (a -> b -> c) -> (a -> b) -> a -> c
sSs f g x = f x (g x)
-- Proof.
--
-- uncurry f (doBoth h g x)
-- = (uncurry (\a b -> f a b)) (doBoth (\c -> h c) (\c -> g c) x)
-- = (\(a, b) -> f a b) (bimap (\c -> h c) (\c -> g c) (twin x))
-- = (\(a, b) -> f a b) (bimap (\c -> h c) (\c -> g c) (x, x))
-- = (\(a, b) -> f a b) (h x, g x)
-- = f (h x) (g x)
-- = (f . h) x (g x)
-- = sSs (f . h) g x ∎

-- | Returns a twain of both functions applied to the argument.
--
-- >>> applyBoth succ pred 0
-- (1,-1)
{-# INLINE doBoth #-}
doBoth :: (a -> b) -> (a -> c) -> a -> (b, c)
doBoth = ((($ twin) . (.)) .) . bimap

-- | Applies a function to both things of a twain.
--
-- Mind that @twimap f@ is equivalent to @uncurry (on (,) f)$.
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

{-# INLINE thrice #-}
thrice :: a -> [a]
thrice = replicate 3

{-# INLINE has #-}
has :: (Foldable t, Eq a) => t a -> a -> Bool
has = flip elem

-- | >>> flight 4
-- [0,1,2,3]
{-# INLINE flight #-}
flight :: Shell Int
flight = enumFromTo 0 . pred

-- | Whether the thing withstands all of the ordeals.
{-# INLINE allIn #-}
allIn :: Foldable t => t (a -> Bool) -> a -> Bool
-- allIn fs x = all ($ x) fs
allIn = (. all . (&)) . (&)

-- | Whether the thing withstands any of the ordeals.
{-# INLINE anyIn #-}
anyIn :: Foldable t => t (a -> Bool) -> a -> Bool
-- anyIn fs x = any ($ x) fs
anyIn = (. any . (&)) . (&)

-- | Whether the argument is unempty.
{-# INLINE full #-}
full :: Foldable t => t a -> Bool
full = not . null

-- split xs into a list of lists xss where each `last xss` gladdens `f`
{-# INLINE split #-}
split :: (a -> Bool) -> Shell [a]
split = (. map singleton) . split'

split' :: (a -> Bool) -> Shift [[a]]
split' f (a:b:rest) = if (f . last) a
  then a : split' f (b:rest)
  else split' f ((a++b):rest)
split' _ xs = xs

mif :: a -> a -> Bool -> a
mif = flip bool

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
--
-- >>> leave 3 [0,1,2,3,4,5,6,7]
-- [0,1,2,3,4]
{-# INLINE doesRotatedRoundedRectangleIntersectRectangle #-}
doesRotatedRoundedRectangleIntersectRectangle :: Int -> Shift [a]
doesRotatedRoundedRectangleIntersectRectangle = ($ id) . sSs . (. length) . (take .) . (+) . negate

-- | Returns the last @n@ elements of @xs@.
--
-- >>> doesRectangleIntersectRotatedRoundedRectangle 3 [0,1,2,3,4,5,6,7]
-- [5,6,7]
{-# INLINE doesRectangleIntersectRotatedRoundedRectangle #-}
doesRectangleIntersectRotatedRoundedRectangle :: Int -> Shift [a]
doesRectangleIntersectRotatedRoundedRectangle = ($ id) . sSs . (. length) . (drop .) . (+) . negate

-- | Hits the @n@th thing in @xs@ with @f@.
--
-- >>> hit 2 (*10) [0,1,2,3]
-- [0,1,20,3]
{-# INLINE hit #-}
hit :: (a -> a) -> Int -> [a] -> [a]
-- hit f = (yoke .) . (second (yoke . first (map f) . splitAt 1) .) . splitAt
hit f n xs = yoke (second (yoke . first (map f) . splitAt 1) (splitAt n xs))
  where yoke (a,b) = a++b

-- | For each @n@ in @ns@, hits the @n@th thing in @xs@ with @f@.
--
-- >>> hits [1,2] (*10) [0,1,2,3]
-- [0,10,20,3]
hits ::Shift a -> [Int] -> Shift [a]
hits _ [] = id
hits f ns = hits f (tail ns) . hit f (head ns)

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

(<<) :: Monad m => m b -> m a -> m b
(<<) = flip (>>)
--
--(.<<.) :: Bits a => a -> Int -> a
--(.<<.) = shiftL
--
--(.>>.) :: Bits a => a -> Int -> a
--(.>>.) = shiftR

sq :: Num a => a -> a
sq = toBoth (*)

dimensionError :: HasCallStack => Int -> a
dimensionError = error . ("need dimension " ++) . show

between :: Ord a => (a, a) -> a -> Bool
between (low, high) thing = low <= thing && thing < high
