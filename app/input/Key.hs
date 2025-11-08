{- HLINT ignore "Use infix" -}

module Key where

import Control.Lens (makeLenses, (^.))

import SDL (InputMotion (Pressed, Released), Event)
import SDL.Input.Keyboard.Codes

import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2))

import Happen (Keywit, unwrapHappenKeys)
import Matrix (Point)
import Mean (allIn)


data KeySet = KeySet {
  _keysBegin :: [Scancode]
, _keysContinue :: [Scancode]
, _keysEnd :: [Scancode]
} deriving (Eq)
makeLenses ''KeySet

instance Show KeySet where
  show ks = concatMap (show . map unwrapScancode . (ks^.)) [keysBegin, keysContinue, keysEnd]

hearableKeys :: [Scancode]
hearableKeys = [
    ScancodeLeft
  , ScancodeRight
  , ScancodeUp
  , ScancodeDown
  , ScancodeW
  , ScancodeA
  , ScancodeS
  , ScancodeD
  , ScancodeSpace
  , ScancodeReturn
  , ScancodeP -- pause
  , ScancodeQ -- quit
  , ScancodeT -- show time
  , ScancodeK -- show keys
  ]

unkeys :: KeySet
unkeys = KeySet [] [] []

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
keyContinuing keySet code = elem code (keySet^.keysBegin ++ keySet^.keysContinue)

-- | Checks @keySet@ to see if @code@ ended being depressed on this frame.
keyEnded :: KeySet -> Scancode -> Bool
keyEnded keySet code = elem code (keySet^.keysEnd)

listen :: [Event] -> KeySet -> KeySet
listen events keyset = let news = unwrapHappenKeys events in KeySet
  (filter (allIn [keyDown news, not.keyContinuing keyset]) hearableKeys)
  (filter (allIn [not.keyUp news, keyContinuing keyset]) hearableKeys)
  (filter (allIn [keyUp news, keyContinuing keyset]) hearableKeys)

wayUpDown :: KeySet -> (KeySet -> Scancode -> Bool) -> GLfloat
wayUpDown = way' (ScancodeW, ScancodeS)

wayLeftRight :: KeySet -> (KeySet -> Scancode -> Bool) -> GLfloat
wayLeftRight = way' (ScancodeA, ScancodeD)

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
