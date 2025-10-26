module Key where

import SDL
import Foreign.C
import Mean
import Data.Set

type Keysuch = Scancode -> Bool

type Keys = (KeysDown, KeysUp)
type KeysDown = Set Scancode
type KeysUp = Set Scancode

unkeys :: Keys
unkeys = (empty, empty)

wayward :: Keysuch -> V3 CFloat
wayward ks = normalize $ V3
  ((cast.ks) ScancodeRight - (cast.ks) ScancodeLeft)
  ((cast.ks) ScancodeDown  - (cast.ks) ScancodeUp  )
  0