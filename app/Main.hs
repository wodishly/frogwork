{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import SDL hiding (trace)
import Foreign.C
import Control.Monad
import System.Random
import Control.Lens

import Mean
import Random
import Light
import Key
import World

data GameOptions = GameOptions {
  _meting :: Bool
, _paused :: Bool
}

data GameState = GameState {
  _frame :: Int
, _seed :: Seed
, _options :: GameOptions
, _position :: V2 CFloat
}

defaultOptions :: GameOptions
defaultOptions = GameOptions True False

defaultState :: GameState
defaultState = GameState 0 defaultSeed defaultOptions (lower center)

openGLWindow :: WindowConfig
openGLWindow = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }

instance Show GameOptions where
  show go = concatMap ($ go) [show._meting, show._paused]

instance Show GameState where
  show gs = concatMap ($ gs) [show._frame, show._options]

makeLenses ''GameOptions
makeLenses ''GameState

tick :: GameState -> GameState
tick gs = set frame (succ $ gs^.frame) gs

toggle :: GameState -> (GameOptions -> Bool) -> GameState
toggle gs go = set (options.meting) (not $ gs^.options.to go) gs

main :: IO ()
main = do
  initializeAll
  window <- createWindow "frog universe" openGLWindow
  renderer <- createRenderer window (-1) defaultRenderer
  live renderer defaultState
  die window
  quit

die :: Window -> IO ()
die window = do
  destroyWindow window
  pollEvents
  return ()

live :: Renderer -> GameState -> IO ()
live renderer state = do
  when (state^.options.meting) (print $ state^.frame)

  es <- pollEvents
  ks <- getKeyboardState
  state <- understand ks state

  if state^.options.paused
  then pauseState renderer state
  else playState renderer state

  present renderer

  unless (ks ScancodeQ) (live renderer (tick state))

understand :: (Scancode -> Bool) -> GameState -> IO GameState
understand ks gs = do
  gs <- return $ set (options.meting) (ks ScancodeT) gs
  gs <- return $ set (options.paused) (ks ScancodeP) gs
  return $ move ks gs

move :: Keysuch -> GameState -> GameState
move ks gs = if gs^.options.paused
  then gs
  else set position ((gs^.position) + (lower.wayward) ks) gs

playState :: Renderer -> GameState -> IO GameState
playState renderer state = do
  rendererDrawColor renderer $= black
  clear renderer
  rendererDrawColor renderer $= green
  fillRect renderer (Just $ Rectangle (P $ round <$> state^.position) (lower $ round <$> size frog))
  return state

pauseState :: Renderer -> GameState -> IO GameState
pauseState renderer state = do
  rendererDrawColor renderer $= blue
  clear renderer
  return state