module FastenMain where

import SDL (
  AudioDeviceUsage (ForPlayback),
  AudioFormat (FloatingNativeAudio),
  Changeable (Desire),
  Channels (Stereo),
  Mode (Normal),
  OpenDeviceSpec (..),
  Profile (Core),
  V2 (V2),
  V4 (V4),
  )

import qualified SDL (
  OpenGLConfig (..),
  WindowConfig (..),
  WindowGraphicsContext (OpenGLContext),
  defaultWindow
  )
import qualified Graphics.Rendering.OpenGL as GL (Vertex2 (Vertex2))


data Stake = North | South | East | West | Middle deriving (Show, Eq)
type Stakes = (Stake, Stake)

shaderBasePath :: String
shaderBasePath = "app/render/shaders"

assetsBasePath :: String
assetsBasePath = "assets"

wayToFeathers :: String
wayToFeathers = assetsBasePath ++ "/feathers"

stillness :: OpenDeviceSpec
stillness = OpenDeviceSpec {
  openDeviceFreq = Desire 44100,
  openDeviceFormat = Desire FloatingNativeAudio,
  openDeviceChannels = Desire Stereo,
  openDeviceSamples = 2^11, --bendsome
  openDeviceUsage = ForPlayback,
  openDeviceName = Nothing,
  openDeviceCallback = \_ _ -> return ()
}


openGLConfig :: SDL.OpenGLConfig
openGLConfig = SDL.OpenGLConfig {
  SDL.glColorPrecision = V4 8 8 8 0,
  SDL.glDepthPrecision = 24,
  SDL.glStencilPrecision = 8,
  SDL.glMultisampleSamples = 1,
  SDL.glProfile = Core Normal 2 1
}

openGLWindow :: SDL.WindowConfig
openGLWindow = SDL.defaultWindow {
  SDL.windowGraphicsContext = SDL.OpenGLContext openGLConfig,
  SDL.windowResizable = True,
  SDL.windowHighDPI = True,
  SDL.windowInitialSize = SDL.V2 orwidth orheight
}

orwindow :: Num a => GL.Vertex2 a
orwindow = GL.Vertex2 orwidth orheight

orwest :: Fractional a => GL.Vertex2 a
orwest = GL.Vertex2 0 (orheight/2)

orwidth :: Num a => a
orwidth = 800

orheight :: Num a => a
orheight = 600

staveSharpness :: Num a => a
staveSharpness = 2^7
