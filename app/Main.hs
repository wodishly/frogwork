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
main = do
  initializeAll
  w <- createWindow "monads" defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
  r <- createRenderer w (-1) defaultRenderer
  live r (randoms $ mkStdGen 0 :: Seed) 0 center
  die w
  quit

die :: Window -> IO ()
die w = do
  destroyWindow w
  pollEvents
  return ()

live :: Renderer -> Seed -> Int -> V2 CFloat -> IO ()
live rend rands t z = do
  es <- pollEvents
  ks <- getKeyboardState
  rendererDrawColor rend $= black
  clear rend
  rendererDrawColor rend $= green
  fillRect rend (Just $ Rectangle (P $ fmap round z) (fmap round $ size glee))
  present rend
  unless (ks ScancodeQ) (live rend rands (succ t) (z + wayward ks))