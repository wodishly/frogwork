{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import SDL
import Foreign.C
import Control.Lens
import Control.Monad

import Test
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
  _seed :: Seed
, _options :: GameOptions
, _position :: V2 CFloat
, _keys :: Keys
}

defaultOptions :: GameOptions
defaultOptions = GameOptions False False

defaultState :: GameState
defaultState = GameState defaultSeed defaultOptions (lower center) unkeys

openGLWindow :: WindowConfig
openGLWindow = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }

instance Show GameOptions where
  show go = concatMap ($ go) [show._meting, show._paused]

instance Show GameState where
  show = show . _options

makeLenses ''GameOptions
makeLenses ''GameState

toggle :: GameState -> (GameOptions -> Bool) -> GameState
toggle gs go = set (options.meting) (not $ gs^.options.to go) gs

main :: IO ()
main = do
  runTests
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
  when (state^.options.meting) $ ticks >>= print

  es <- pollEvents
  ks <- getKeyboardState
  state <- understand ks state

  if state^.options.paused
  then pauseState renderer state
  else playState renderer state

  present renderer

  unless (ks ScancodeQ) (live renderer state)

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