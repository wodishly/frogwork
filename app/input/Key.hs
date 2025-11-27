module Key (
  module Key,
  module SDL.Input.Keyboard.Codes,
) where

import SDL.Input.Keyboard.Codes

import Happen
import Rime
import Mean


data Keyset = Keyset {
  begunKeys :: [Scancode],
  continuingKeys :: [Scancode],
  endedKeys :: [Scancode]
} deriving (Eq)

instance Show Keyset where
  show (Keyset b c e) = concatMap (show . map unwrapScancode) [b, c, e]

unlockKeys :: IO ()
unlockKeys = void getKeyboardState

lockKeys :: IO ()
lockKeys = return ()

hearableKeys :: [Scancode]
hearableKeys = [
  ScancodeLeft, -- arrows to control camera
  ScancodeRight,
  ScancodeUp,
  ScancodeDown,

  ScancodeW, -- wasd to move
  ScancodeA,
  ScancodeS,
  ScancodeD,

  ScancodeSpace, -- leap
  ScancodeLShift, -- run
  ScancodeRShift, -- run

  ScancodeTab, -- show spell
  ScancodeReturn, -- choose
  ScancodeEscape, -- quit
  ScancodeP, -- pause
  ScancodeQ, -- quit
  ScancodeR, -- restart

  ScancodeK, -- show keys
  ScancodeT, -- show time
  ScancodeM  -- be loud
  ]

unkeys :: Keyset
unkeys = Keyset [] [] []

-- | Checks @wits@ to see if @code@ is @Pressed@.
keyDown :: [Keywit] -> Scancode -> Bool
keyDown = (. (, Pressed)) . has

-- | Checks @wits@ to see if @code@ is @Released@.
keyUp :: [Keywit] -> Scancode -> Bool
keyUp = (. (, Released)) . has

-- | Checks if the given code began being depressed on this frame.
keyBegun :: Keyset -> Scancode -> Bool
keyBegun = has . begunKeys

-- | Like @keyBegun@, but for a set of codes.
anyKeysBegun :: Keyset -> [Scancode] -> Bool
anyKeysBegun = any . keyBegun

-- | Checks if the given code continues being depressed since an earlier frame.
keyContinuing :: Keyset -> Scancode -> Bool
keyContinuing = has . sSs ((++) . begunKeys) continuingKeys

-- | Like @keyContinuing@, but for a set of codes.
anyKeysContinuing :: Keyset -> [Scancode] -> Bool
anyKeysContinuing = any . keyContinuing

-- | Checks if the given code ended being depressed on this frame.
keyEnded :: Keyset -> Scancode -> Bool
keyEnded = has . endedKeys

bethinkKeys :: [Event] -> Keyset -> Keyset
bethinkKeys events keyset = let news = unwrapHappenKeys events in Keyset
  (filter (allIn [keyDown news, not . keyContinuing keyset]) hearableKeys)
  (filter (allIn [not . keyUp news, keyContinuing keyset]) hearableKeys)
  (filter (allIn [keyUp news, keyContinuing keyset]) hearableKeys)

-- | Uses a 2-tuple of antipodal keycodes to compute a unit direction vector.
way' :: ([Scancode], [Scancode]) -> Keyset -> (Keyset -> Scancode -> Bool) -> GLfloat
way' (wanes, waxes) keyset keyListener
  | any (keyListener keyset) wanes && none (keyListener keyset) waxes = -1
  | any (keyListener keyset) waxes && none (keyListener keyset) wanes = 1
  | otherwise = 0

arrow :: Keyset -> Point
arrow keySet = hat $ Vertex2
  (way' ([ScancodeLeft], [ScancodeRight]) keySet keyContinuing)
  (way' ([ScancodeUp], [ScancodeDown]) keySet keyContinuing)

wasd :: Keyset -> Point
wasd keySet = hat $ Vertex2
  (way' ([ScancodeA], [ScancodeD]) keySet keyContinuing)
  (way' ([ScancodeW], [ScancodeS]) keySet keyContinuing)
