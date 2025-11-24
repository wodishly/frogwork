module FastenFrame where

import Rime (FrogVertex (nonehood, onehood, (^*^)))
import Strike (Spit)
import Control.Arrow (Arrow(second))
import Graphics.Rendering.OpenGL (Vertex3(Vertex3))

{-# INLINE onespit #-}
onespit :: Spit
onespit = (nonehood, onehood)

{-# INLINE tallspit #-}
tallspit :: Spit
tallspit = second (^*^ Vertex3 1 2 1) onespit
