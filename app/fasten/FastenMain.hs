module FastenMain where

import SDL (Mode (Normal), Profile (Core), V2 (V2), V4 (V4))
import qualified SDL (
    OpenGLConfig (..)
  , WindowConfig (..)
  , WindowGraphicsContext (OpenGLContext)
  , defaultWindow
  )
import Graphics.Rendering.OpenGL (Vertex2 (Vertex2))

import Foreign (Word32)
import Mean (Twain)
import Rime (Point)


data Stake = North | South | East | West | Middle deriving (Show, Eq)
type Stakes = Twain Stake

shaderBasePath :: String
shaderBasePath = "app/render/shaders"

assetsBasePath :: String
assetsBasePath = "assets"

wayToFeathers :: String
wayToFeathers = assetsBasePath ++ "/feathers"

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
  , SDL.windowHighDPI = True
  , SDL.windowInitialSize = SDL.V2 orwidth orheight
}

orwindow :: Point
orwindow = Vertex2 orwidth orheight

orwest :: Point
orwest = Vertex2 0 (orheight/2)

orwidth :: Num a => a
orwidth = 800

orheight :: Num a => a
orheight = 600

staveSharpness :: Word32
staveSharpness = 2^7
