module FastenMain where

import SDL (Mode (Normal), Profile (Core), V4 (V4))
import qualified SDL (
    OpenGLConfig (..)
  , WindowConfig (..)
  , WindowGraphicsContext (OpenGLContext)
  , defaultWindow
  )


framegoal :: Num a => a
framegoal = 60

framefulness :: Num a => a
framefulness = 3

shaderBasePath :: String
shaderBasePath = "app/render/shaders"

assetsBasePath :: String
assetsBasePath = "assets"

openGLConfig :: SDL.OpenGLConfig
openGLConfig = SDL.OpenGLConfig {
    SDL.glColorPrecision = V4 8 8 8 0
  , SDL.glDepthPrecision = 24
  , SDL.glStencilPrecision = 8
  , SDL.glMultisampleSamples = 1
  , SDL.glProfile = Core Normal 2 1
}

openGLWindow :: SDL.WindowConfig
openGLWindow = SDL.defaultWindow {
    SDL.windowGraphicsContext = SDL.OpenGLContext openGLConfig
  , SDL.windowResizable = True
  , SDL.windowInputGrabbed = False
}
