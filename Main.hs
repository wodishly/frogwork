-- stack ghci --ghci-options '-fno-ghci-sandbox' Main.hs
-- :set -package lens
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import SDL hiding (trace)
import Foreign.C
import Control.Monad
import System.Random

import Mean
import Light
import World

main :: IO ()
main = initializeAll
  >> createWindow "monads" defaultWindow
  >>= (\w -> createRenderer w (-1) defaultRenderer
    >>= (\r -> live r (randoms $ mkStdGen 0 :: Seed) 0 navel)
    >> die w
  ) >> quit

die :: Window -> IO ()
die w = destroyWindow w >> pollEvents >> return ()

live :: Renderer -> Seed -> Int -> V2 CFloat -> IO ()
live rend rands t z = print t
  >> pollEvents
  >>= (\es -> getKeyboardState
    >>= (\ks -> (
        rendererDrawColor rend $= black
        >> clear rend
      >> rendererDrawColor rend $= green
        >> fillRect rend (Just $ Rectangle (P $ fmap round z) (fmap round $ girth glee))
    ) >> present rend
      >> unless (ks ScancodeQ) (live rend rands (succ t) (z + wayward ks))
    )
  )