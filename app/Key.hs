module Key where
import SDL
import Foreign.C
import Mean
  
type Keysuch = Scancode -> Bool

wayward :: Keysuch -> V3 CFloat
wayward ks = normalize $ V3
  ((cast.ks) ScancodeRight - (cast.ks) ScancodeLeft)
  ((cast.ks) ScancodeDown  - (cast.ks) ScancodeUp  )
  0