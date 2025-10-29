{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import SDL
import Control.Lens
import Control.Monad

import Test
import Key
import State

openGLWindow :: WindowConfig
openGLWindow = defaultWindow {
  windowGraphicsContext = OpenGLContext defaultOpenGL
}

main :: IO ()
main = do
  when (defaultState^.options.isRunningTests) allfand

  initializeAll
  window <- createWindow "frog universe" openGLWindow
  renderer <- createRenderer window (-1) defaultRenderer
  live renderer defaultState
  die window
  quit

die :: Window -> IO ()
die window = do
  destroyWindow window
  _ <- pollEvents
  return ()

live :: Renderer -> StateInfo -> IO ()
live renderer stateInfo = do
  events <- pollEvents
  keys <- listen (stateInfo^.keyset) <$> getKeyboardState

  when (stateInfo^.options.isShowingKeys) (print keys)
  when (stateInfo^.options.isShowingTicks) (ticks >>= print)

  stateInfo <- understand keys events stateInfo
  stateInfo <- (stateByName $ stateInfo^.currentState) renderer keys events stateInfo

  present renderer

  unless (keyBegun keys ScancodeQ) (live renderer stateInfo)