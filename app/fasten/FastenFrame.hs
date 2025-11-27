module FastenFrame where

import Mean
import Rime
import Strike


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
