{-# LANGUAGE TemplateHaskell #-}
{- HLINT ignore "Use infix" -}

module Key where

import Control.Lens

import SDL.Input.Keyboard.Codes

import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2))

import Light
import Mean
import SDL (Event (eventPayload), Keysym (keysymScancode), KeyboardEventData (keyboardEventKeysym, keyboardEventKeyMotion), InputMotion (Pressed, Released))
import SDL.Event (EventPayload(KeyboardEvent))
import Data.Maybe (mapMaybe)

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

type Keywit = (Scancode, InputMotion)

-- | Checks @wits@ to see if @code@ is @Pressed@.
keyDown :: [Keywit] -> Scancode -> Bool
keyDown wits code = elem (code, Pressed) wits

-- | Checks @wits@ to see if @code@ is @Released@.
keyUp :: [Keywit] -> Scancode -> Bool
keyUp wits code = elem (code, Released) wits

-- | Checks @keySet@ to see if @code@ began being depressed on this frame.
keyBegun :: KeySet -> Scancode -> Bool
keyBegun keySet code = elem code (keySet^.keysBegin)

-- | Checks @keySet@ to see if @code@ continues being depressed since an earlier frame.
keyContinuing :: KeySet -> Scancode -> Bool
keyContinuing keySet = flip elem (keySet^.keysBegin ++ keySet^.keysContinue)

-- | Checks @keySet@ to see if @code@ ended being depressed on this frame.
keyEnded :: KeySet -> Scancode -> Bool
keyEnded keySet = flip elem (keySet^.keysEnd)

unwrapKeys :: [Event] -> [Keywit]
unwrapKeys = mapMaybe (\event -> case eventPayload event of
  KeyboardEvent e -> Just (keysymScancode $ keyboardEventKeysym e, keyboardEventKeyMotion e)
  _ -> Nothing)

listen :: [Event] -> KeySet -> KeySet
listen events keyset = let news = unwrapKeys events in KeySet
  (filter (allIn [keyDown news, not.keyContinuing keyset]) hearableKeys)
  (filter (allIn [not.keyUp news, keyContinuing keyset]) hearableKeys)
  (filter (allIn [keyUp news, keyContinuing keyset]) hearableKeys)

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