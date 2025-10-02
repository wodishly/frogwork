{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL hiding (trace)
import Foreign.C
import Data.Word
import Debug.Trace
import Control.Monad

ly :: Show a => a -> a
ly x = trace (show x) x

bint :: Bool -> CInt
bint = fromIntegral.fromEnum

white :: V4 Word8
white = V4 255 255 255 255

green :: V4 Word8
green = V4 0 255 0 255

main :: IO ()
main = initializeAll
  >> createWindow "monads" defaultWindow
  >>= (\x -> createRenderer x (-1) defaultRenderer
    >>= flip (flip loop 0) (0, 0)
    >> destroyWindow x)
  >> quit

loop :: Renderer -> Int -> (CInt, CInt) -> IO ()
loop renderer n xy = pollEvents
  >>= (\events -> getKeyboardState
    >>= (\keys -> (rendererDrawColor renderer $= white
      >> clear renderer >> rendererDrawColor renderer $= green
      >> fillRect renderer (Just (Rectangle (P $ uncurry V2 xy) (V2 64 64)))
      >> present renderer
      >> unless (keys ScancodeQ) (loop renderer (succ n)
        (fst xy + (bint.keys) ScancodeRight - (bint.keys) ScancodeLeft,
         snd xy + (bint.keys) ScancodeDown  - (bint.keys) ScancodeUp)))))