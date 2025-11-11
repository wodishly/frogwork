module Key (
  KeySet
, unkeys
, listen
, keyBegun
, anyKeysBegun
, keyEnded -- unused
, arrow
, wasd
) where

import Control.Arrow ((>>>))

import SDL (InputMotion (Pressed, Released), Event)
import SDL.Input.Keyboard.Codes

import Graphics.Rendering.OpenGL (GLfloat, Vertex2 (Vertex2))

import Happen (Keywit, unwrapHappenKeys)
import Rime (Point, hat)
import Mean (allIn, has, none, doBoth)
import Data.Function ((&))


data KeySet = KeySet {
  begunKeys :: [Scancode]
, continuingKeys :: [Scancode]
, endedKeys :: [Scancode]
} deriving (Eq)

instance Show KeySet where
  show = ($ [begunKeys, continuingKeys, endedKeys]) . concatMap . ((show . map unwrapScancode) .) . (&)

hearableKeys :: [Scancode]
hearableKeys = [
    ScancodeLeft -- arrows to control camera
  , ScancodeRight
  , ScancodeUp
  , ScancodeDown
  , ScancodeW -- wasd to move
  , ScancodeA
  , ScancodeS
  , ScancodeD
  , ScancodeSpace -- leap
  , ScancodeReturn -- choose
  , ScancodeEscape -- quit
  , ScancodeQ -- quit
  , ScancodeP -- pause
  , ScancodeT -- show time
  , ScancodeK -- show keys
  ]

unkeys :: KeySet
unkeys = KeySet [] [] []

-- | Checks @wits@ to see if @code@ is @Pressed@.
keyDown :: [Keywit] -> Scancode -> Bool
keyDown = (>>>) (, Pressed) . has

-- | Checks @wits@ to see if @code@ is @Released@.
keyUp :: [Keywit] -> Scancode -> Bool
keyUp = (>>>) (, Released) . has

-- | Checks if the given code began being depressed on this frame.
keyBegun :: KeySet -> Scancode -> Bool
keyBegun = has . begunKeys

-- | Like @keyBegun@, but for a set of codes.
anyKeysBegun :: KeySet -> [Scancode] -> Bool
anyKeysBegun = any . keyBegun

-- | Checks if the given code continues being depressed since an earlier frame.
keyContinuing :: KeySet -> Scancode -> Bool
keyContinuing = has . uncurry (++) . doBoth begunKeys continuingKeys

-- | Checks if the given code ended being depressed on this frame.
keyEnded :: KeySet -> Scancode -> Bool
keyEnded = has . endedKeys

listen :: [Event] -> KeySet -> KeySet
listen events keyset = let news = unwrapHappenKeys events in KeySet
  (filter (allIn [keyDown news, not.keyContinuing keyset]) hearableKeys)
  (filter (allIn [not.keyUp news, keyContinuing keyset]) hearableKeys)
  (filter (allIn [keyUp news, keyContinuing keyset]) hearableKeys)

-- | Uses a 2-tuple of antipodal keycodes to compute a unit direction vector.
way' :: ([Scancode], [Scancode]) -> KeySet -> (KeySet -> Scancode -> Bool) -> GLfloat
way' (wanes, waxes) keySet keyListener
  | any (keyListener keySet) wanes && none (keyListener keySet) waxes = -1
  | any (keyListener keySet) waxes && none (keyListener keySet) wanes = 1
  | otherwise = 0

arrow :: KeySet -> Point
arrow keySet = hat $ Vertex2
  (way' ([ScancodeLeft], [ScancodeRight]) keySet keyContinuing)
  (way' ([ScancodeUp], [ScancodeDown]) keySet keyContinuing)

wasd :: KeySet -> Point
wasd keySet = hat $ Vertex2
  (way' ([ScancodeA], [ScancodeD]) keySet keyContinuing)
  (way' ([ScancodeW], [ScancodeS]) keySet keyContinuing)
