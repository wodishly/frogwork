{-# LANGUAGE TemplateHaskell #-}

module Key where

import SDL
import Foreign.C
import Control.Lens

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

listen :: KeySet -> (Scancode -> Bool) -> KeySet
listen keySet keyboardState = KeySet
  (filter (allIn [keyboardState, not.keyContinuing keySet]) hearableKeys)
  (filter (allIn [keyboardState, keyContinuing keySet]) hearableKeys)
  (filter (allIn [not.keyboardState, keyContinuing keySet]) hearableKeys)

wayUpDown :: KeySet -> (KeySet -> Scancode -> Bool) -> CFloat
wayUpDown = way' (ScancodeUp, ScancodeDown)

wayLeftRight :: KeySet -> (KeySet -> Scancode -> Bool) -> CFloat
wayLeftRight = way' (ScancodeLeft, ScancodeRight)

way' :: (Scancode, Scancode) -> KeySet -> (KeySet -> Scancode -> Bool) -> CFloat
way' (lower, higher) keySet keyListener
  | keyListener keySet lower = -1
  | keyListener keySet higher = 1
  | otherwise = 0

wayward :: KeySet -> V2 CFloat
wayward keySet = normalize $ V2 (wayLeftRight keySet keyContinuing) (wayUpDown keySet keyContinuing)