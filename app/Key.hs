{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Use infix" #-}

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
keyBegun keyset key = elem key (keyset^.keysBegin)

keyContinuing :: KeySet -> Scancode -> Bool
keyContinuing keyset key = elem key (keyset^.keysBegin ++ keyset^.keysContinue)

keyEnded :: KeySet -> Scancode -> Bool
keyEnded keyset key = elem key (keyset^.keysEnd)

listen :: KeySet -> (Scancode -> Bool) -> KeySet
listen keySet keyboardState = KeySet
  (filter (allIn [keyboardState, not.keyContinuing keySet]) hearableKeys)
  (filter (allIn [keyboardState, keyContinuing keySet]) hearableKeys)
  (filter (allIn [not.keyboardState, keyContinuing keySet]) hearableKeys)

wayward :: KeySet -> V3 CFloat
wayward ks = normalize $ V3
  ((cast.keyContinuing ks) ScancodeRight - (cast.keyContinuing ks) ScancodeLeft)
  ((cast.keyContinuing ks) ScancodeDown  - (cast.keyContinuing ks) ScancodeUp  )
  0