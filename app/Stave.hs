{-# LANGUAGE OverloadedStrings #-}

module Stave where

import Foreign.C
import Control.Monad

import SDL

import World
import Mean
import Data.Char

xscale :: Enum a => a -> V2 CFloat -> V2 CFloat
xscale n (V2 x y) = V2 (cast n*x) y

drawWord :: Renderer -> V2 CFloat -> CFloat -> String -> IO ()
drawWord renderer topLeft size word = forM_ (flight $ length word) $
  \i -> forM_ (flight 9) (\j -> drawStave renderer (cast i *^ V2 (size*3/5) 0 ^+^ cast j ^+^ topLeft) size (word!!i))

drawStave :: Renderer -> V2 CFloat -> CFloat -> Char -> IO ()
drawStave renderer topLeft size c = forM_ (stave topLeft size c) $
  uncurry (drawLine renderer) . twimap (fmap cast)

stave :: V2 CFloat -> CFloat -> Char -> [Pair (Point V2 CFloat)]
stave topLeft size c = map (stave' topLeft size.stavedeal) $ case toLower c of
  'a' -> [0, 1, 2, 4, 5, 6, 7]
  'b' -> [0, 1, 2, 3, 7, 9, 12]
  'c' -> [0, 3, 4, 5]
  'd' -> [0, 1, 2, 3, 9, 12]
  'e' -> [0, 3, 4, 5, 6, 7]
  'f' -> [0, 4, 5, 6, 7]
  'g' -> [0, 2, 3, 4, 5, 7]
  'h' -> [1, 2, 4, 5, 6, 7]
  'i' -> [0, 3, 9, 12]
  'j' -> [1, 2, 3, 4]
  'k' -> [4, 5, 6, 10, 13]
  'l' -> [3, 4, 5]
  'm' -> [1, 2, 4, 5, 8, 10]
  'n' -> [1, 2, 4, 5, 8, 13]
  'o' -> [0, 1, 2, 3, 4, 5]
  'p' -> [0, 1, 4, 5, 6, 7]
  'q' -> [0, 1, 2, 3, 4, 5, 13]
  'r' -> [0, 1, 4, 5, 6, 7, 13]
  's' -> [0, 2, 3, 7, 8]
  't' -> [0, 9, 12]
  'u' -> [1, 2, 3, 4, 5]
  'v' -> [4, 5, 10, 11]
  'w' -> [1, 2, 4, 5, 11, 13]
  'x' -> [8, 10, 11, 13]
  'y' -> [8, 10, 12]
  'z' -> [0, 3, 10, 11]
  _ -> []

stave' :: V2 CFloat -> CFloat -> StaveDeal -> Pair (Point V2 CFloat)
stave' topLeft size = twimap (P . (topLeft ^+^) . ((size/4) *^) . uncurry V2)

type StaveDeal = Pair (Pair CFloat)
stavedeal :: Int -> StaveDeal
stavedeal n = case n of
  0  -> ((0, 0), (2, 0))
  1  -> ((2, 0), (2, 2))
  2  -> ((2, 2), (2, 4))
  3  -> ((0, 4), (2, 4))
  4  -> ((0, 2), (0, 4))
  5  -> ((0, 0), (0, 2))
  6  -> ((0, 2), (1, 2))
  7  -> ((1, 2), (2, 2))
  8  -> ((0, 0), (1, 2))
  9  -> ((1, 0), (1, 2))
  10 -> ((2, 0), (1, 2))
  11 -> ((1, 2), (0, 4))
  12 -> ((1, 2), (1, 4))
  13 -> ((1, 2), (2, 4))
  _ -> error "bad stavedeal"