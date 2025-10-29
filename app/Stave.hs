{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Stave where

import Data.Char
import Foreign.C
import Control.Lens
import Control.Monad

import SDL

import Mean
import Light

data Feather = Feather {
  _featherSize :: CFloat
, _featherThick :: CFloat
, _featherColor :: Color
}
makeLenses ''Feather

defaultFeather :: Feather
defaultFeather = Feather {
  _featherSize = 64
, _featherThick = 4
, _featherColor = yellow
}

featherGrid :: Feather -> [V2 CFloat]
featherGrid feather =
  map (\i -> (0.5 - cast n/2.0) ^+^ V2 (cast $ div i n) (cast $ mod i n)) (flight $ n*n)
  where n = cast $ feather^.featherThick

xscale :: Enum a => a -> V2 CFloat -> V2 CFloat
xscale n (V2 x y) = V2 (cast n*x) y

drawWord :: Renderer -> Feather -> V2 CFloat -> String -> IO ()
drawWord renderer feather topLeft word = forM_ (flight $ length word)
  $ \i -> forM_ (featherGrid feather)
  $ \j -> drawStave renderer feather
    (cast i *^ V2 (3/5*(feather^.featherSize + feather^.featherThick)) 0 ^+^ j ^+^ topLeft)
    (word!!i)

drawStave :: Renderer -> Feather -> V2 CFloat -> Char -> IO ()
drawStave renderer feather topLeft c = forM_ (stave feather topLeft c)
  $ uncurry (drawLine renderer) . twimap (fmap cast)

stave :: Feather -> V2 CFloat -> Char -> [Twain (Point V2 CFloat)]
stave feather topLeft c = map (stave' feather topLeft . stavedeal) $ case toLower c of
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

stave' :: Feather -> V2 CFloat -> StaveDeal -> Twain (Point V2 CFloat)
stave' feather topLeft = twimap (P . (topLeft ^+^) . ((feather^.featherSize/4) *^) . uncurry V2)

type StaveDeal = Twain (Twain CFloat)
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