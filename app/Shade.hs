{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Shade where

import Data.ByteString as BS (readFile)
import Foreign (Ptr, nullPtr, plusPtr, withArray, sizeOf)
import Control.Monad (unless)
import Graphics.Rendering.OpenGL

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr . fromIntegral

shade :: [Vertex2 GLfloat] -> IO ()
shade vertices = do
  vertexShader <- createShader VertexShader
  vertexSource <- BS.readFile "app/shaders/vertex.glsl"
  shaderSourceBS vertexShader $= vertexSource
  compileShader vertexShader
  fragmentShader <- createShader FragmentShader
  fragmentSource <- BS.readFile "app/shaders/fragment.glsl"
  shaderSourceBS fragmentShader $= fragmentSource
  compileShader fragmentShader
  vertexStatus <- get (compileStatus vertexShader)
  fragmentStatus <- get (compileStatus fragmentShader)

  vsLog <- get $ shaderInfoLog vertexShader
  putStrLn vsLog
  print vertexSource
  print vertexStatus

  unless vertexStatus (error "vertex shader failed to compile :(")
  unless fragmentStatus (error "fragment shader failed to compile :(")

  program <- createProgram
  attachShader program vertexShader
  attachShader program fragmentShader
  bindFragDataLocation program "color" $= 0
  linkProgram program
  linkStatus <- get (linkStatus program)
  unless linkStatus (error "fuck")

  vao <- genObjectName
  bindVertexArrayObject $= Just vao
  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo

  let size = fromIntegral $ length vertices * sizeOf (head vertices)
  withArray vertices $ \ptr -> bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled
  currentProgram $= Just program
