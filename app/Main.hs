{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import SDL hiding (Pause, Play)
import Control.Lens
import Control.Monad
import Graphics.Rendering.OpenGL.GL as GL
import Data.ByteString as BS

import Test
import Key
import State
import MenuState
import Foreign.Ptr
import Foreign (Storable(sizeOf), withArray)


myGlConfig :: OpenGLConfig
myGlConfig = OpenGLConfig {
    glColorPrecision = V4 8 8 8 0
  , glDepthPrecision = 24
  , glStencilPrecision = 8
  , glMultisampleSamples = 1,
    glProfile = Core Normal 2 1
}

openGLWindow :: WindowConfig
openGLWindow = defaultWindow {
  windowGraphicsContext = OpenGLContext myGlConfig
}

main :: IO ()
main = do
  when (defaultState^.options.isRunningTests) allfand

  initializeAll

  window <- createWindow "frog universe" openGLWindow
  ctx <- glCreateContext window



  vertexShader <- GL.createShader VertexShader
  vertexSource <- BS.readFile "app/shaders/vertex.glsl"
  GL.shaderSourceBS vertexShader $= vertexSource
  GL.compileShader vertexShader
  fragmentShader <- GL.createShader FragmentShader
  fragmentSource <- BS.readFile "app/shaders/fragment.glsl"
  GL.shaderSourceBS fragmentShader $= fragmentSource
  GL.compileShader fragmentShader
  vertexStatus <- GL.get (GL.compileStatus vertexShader)
  fragmentStatus <- GL.get (GL.compileStatus fragmentShader)

  vslog <- GL.get $ GL.shaderInfoLog vertexShader
  putStrLn vslog
  print vertexSource
  print vertexStatus

  unless vertexStatus (error "vertex shader failed to compile :(")
  unless fragmentStatus (error "fragment shader failed to compile :(")

  program <- GL.createProgram
  GL.attachShader program vertexShader
  GL.attachShader program fragmentShader
  GL.bindFragDataLocation program "color" $= 0
  GL.linkProgram program
  linkStatus <- GL.get (GL.linkStatus program)
  unless linkStatus (error "fuck")

  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  vbo <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vbo

  let numVertices = fromIntegral $ Prelude.length vertices
      size        = fromIntegral $ numVertices * sizeOf (Prelude.head vertices)
  withArray vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (size, ptr, GL.StaticDraw)


  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    (GL.ToFloat,
    GL.VertexArrayDescriptor
    2
    GL.Float
    0
    (bufferOffset 0))
  GL.vertexAttribArray (AttribLocation 0) $= Enabled

  GL.currentProgram $= Just program

  live2 window ctx defaultState
  -- renderer <- createRenderer window (-1) defaultRenderer
  -- live renderer defaultState

  GL.finish
  glDeleteContext ctx
  die window
  quit

vertices :: [GL.Vertex2 GL.GLfloat]
vertices = [ GL.Vertex2    0.0    0.5 
            , GL.Vertex2   0.5  (-0.5)
            , GL.Vertex2 (-0.5) (-0.5) ]

bufferOffset :: Integral a => a -> Ptr a
bufferOffset = plusPtr nullPtr . fromIntegral

live2 :: Window -> GLContext -> StateInfo -> IO ()
live2 window ctx stateInfo = do
  events <- pollEvents
  keys <- listen (stateInfo^.keyset) <$> getKeyboardState


  let numVertices = fromIntegral $ Prelude.length vertices
      size        = fromIntegral $ numVertices * sizeOf (Prelude.head vertices)
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer]
  V2 winWidth winHeight <- SDL.get (SDL.windowSize window)
  GL.viewport $= (GL.Position 0 0,
                  GL.Size (fromIntegral winWidth) (fromIntegral winHeight))
  GL.drawArrays GL.Triangles 0 (fromIntegral numVertices)
  SDL.glSwapWindow window

  unless (keyBegun keys ScancodeQ) (live2 window ctx stateInfo)

die :: Window -> IO ()
die window = do
  destroyWindow window
  _ <- pollEvents
  return ()

stateByName :: StateName -> GameState
stateByName name = case name of
  Play -> playState
  Pause -> pauseState
  Menu -> menuState
  _ -> error "bad state"

live :: Renderer -> StateInfo -> IO ()
live renderer stateInfo = do
  events <- pollEvents
  keys <- listen (stateInfo^.keyset) <$> getKeyboardState

  when (stateInfo^.options.isShowingKeys) (print keys)
  when (stateInfo^.options.isShowingTicks) (ticks >>= print)

  stateInfo <- understand keys events stateInfo
  stateInfo <- (stateByName $ stateInfo^.currentState) renderer keys events stateInfo

  present renderer

  unless (keyBegun keys ScancodeQ) (live renderer stateInfo)