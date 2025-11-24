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

{-# INLINE longspit #-}
longspit :: Spit
longspit = second (^*^ Vertex3 1 1 2) onespit

{-# INLINE widespit #-}
widespit :: Spit
widespit = second (^*^ Vertex3 2 1 1) onespit
