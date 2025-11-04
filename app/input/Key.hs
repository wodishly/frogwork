{-# LANGUAGE TemplateHaskell #-}

module Key where

import Control.Lens

import SDL.Input.Keyboard.Codes

import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2))

import Light
import Mean

data KeySet = KeySet {
  _keysBegin :: [Scancode]
, _keysContinue :: [Scancode]
, _keysEnd :: [Scancode]
}
makeLenses ''KeySet

instance Show KeySet where
  show ks = concatMap (show . map unwrapScancode . (ks^.)) [keysBegin, keysContinue, keysEnd]

hearableKeys :: [Scancode]
hearableKeys = [
    ScancodeLeft
  , ScancodeRight
  , ScancodeUp
  , ScancodeDown
  , ScancodeReturn
  , ScancodeP -- pause
  , ScancodeQ -- quit
  , ScancodeT -- show time
  , ScancodeK -- show keys
  ]

unkeys :: KeySet
unkeys = KeySet [] [] []

keyBegun :: KeySet -> Scancode -> Bool
keyBegun keySet = flip elem (keySet^.keysBegin)

keyContinuing :: KeySet -> Scancode -> Bool
keyContinuing keySet = flip elem (keySet^.keysBegin ++ keySet^.keysContinue)

keyEnded :: KeySet -> Scancode -> Bool
keyEnded keySet = flip elem (keySet^.keysEnd)

listen :: KeySet -> [Scancode] -> KeySet
listen olds news = KeySet
  (filter (allIn [flip elem news, not.keyContinuing olds]) hearableKeys)
  (filter (allIn [flip elem news, keyContinuing olds]) hearableKeys)
  (filter (allIn [not.flip elem news, keyContinuing olds]) hearableKeys)

wayUpDown :: KeySet -> (KeySet -> Scancode -> Bool) -> GLfloat
wayUpDown = way' (ScancodeUp, ScancodeDown)

wayLeftRight :: KeySet -> (KeySet -> Scancode -> Bool) -> GLfloat
wayLeftRight = way' (ScancodeLeft, ScancodeRight)

way' :: (Scancode, Scancode) -> KeySet -> (KeySet -> Scancode -> Bool) -> GLfloat
way' (lower, higher) keySet keyListener
  | keyListener keySet lower = -1
  | keyListener keySet higher = 1
  | otherwise = 0

wayward :: KeySet -> Point
wayward keySet = hat $ Vertex2 (wayLeftRight keySet keyContinuing) (wayUpDown keySet keyContinuing)

norm :: Point -> GLfloat
norm (Vertex2 x y) = sqrt (x*x + y*y)

hat :: Point -> Point
hat z = if norm z == 0 then z else fmap (/norm z) z