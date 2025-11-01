{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Monad (unless, when)
import Control.Lens ((^.), set)

import SDL.Video
import SDL.Event (pollEvents)
import SDL.Input.Keyboard.Codes
import SDL.Vect ( V4(V4), V2(V2) )
import SDL (initializeAll, quit, getKeyboardState, ticks)
import Graphics.Rendering.OpenGL

import Key
import State
import Test
import MenuState
import Shade
import Time

openGLConfig :: OpenGLConfig
openGLConfig = OpenGLConfig {
    glColorPrecision = V4 8 8 8 0
  , glDepthPrecision = 24
  , glStencilPrecision = 8
  , glMultisampleSamples = 1
  , glProfile = Core Normal 2 1
}

openGLWindow :: WindowConfig
openGLWindow = defaultWindow {
  windowGraphicsContext = OpenGLContext openGLConfig
}

main :: IO ()
main = do
  when (defaultState^.options.isRunningTests) someFand

  initializeAll

  window <- createWindow "frog universe" openGLWindow
  ctx <- glCreateContext window

  _ <- shade defaultState

  V2 winWidth winHeight <- get (windowSize window)
  viewport $= (Position 0 0, Size (fromIntegral winWidth) (fromIntegral winHeight))

  let stateInfo = defaultState
  stateInfo <- birth stateInfo
  live window ctx stateInfo

  die window ctx
  quit

birth :: StateInfo -> IO StateInfo
birth stateInfo = do
  shade stateInfo

live :: Window -> GLContext -> StateInfo -> IO ()
live window ctx stateInfo = do
  events <- pollEvents
  keys <- if waxen (stateInfo^.time)
    then listen (stateInfo^.keyset) <$> getKeyboardState
    else pure unkeys

  now <- ticks
  stateInfo <- pure $ set time (keepTime (stateInfo^.time) now) stateInfo

  when (stateInfo^.options.isShowingKeys) (print keys)
  when (stateInfo^.options.isShowingTicks) (print $ stateInfo^.time)

  stateInfo <- understand keys events stateInfo
  stateInfo <- (stateByName $ stateInfo^.currentState) ctx keys events stateInfo

  glSwapWindow window

  unless (keyBegun keys ScancodeQ) (live window ctx stateInfo)

die :: Window -> GLContext -> IO ()
die window ctx = do
  finish
  glDeleteContext ctx
  destroyWindow window
  _ <- pollEvents
  return ()

stateByName :: StateName -> GameState
stateByName name = case name of
  Play -> playState
  Pause -> pauseState
  Menu -> menuState
  _ -> error "bad state"
