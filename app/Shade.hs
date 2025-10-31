{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Shade where

import Control.Lens.Combinators (set)
import Control.Monad (unless)
import Data.Binary.Get
import Data.ByteString as BS (readFile)
import File
import Foreign (Ptr, nullPtr, plusPtr, sizeOf)
import Foreign.Marshal
import Graphics.Rendering.OpenGL as GL
import State

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr . fromIntegral

shade :: StateInfo -> IO StateInfo
shade stateInfo = do
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
  fsLog <- get $ shaderInfoLog fragmentShader
  putStrLn fsLog

  fbytes <- getFrogBytes
  let mesh = runGet parseFrogFile fbytes
  let vbuffer = positionBuffer mesh
  let ibuffer = indexBuffer mesh
  let uvs = uvBuffer mesh
  let bitmap = bitmapBuffer mesh
  let tw = texWidth mesh
  let th = texHeight mesh

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

  uvbo <- genObjectName
  bindBuffer ArrayBuffer $= Just uvbo
  withArray uvs $ \ptr ->
    bufferData ArrayBuffer $= (fromIntegral (length uvs * sizeOf (head uvs)), ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 1)
    $= (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 1) $= Enabled

  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo
  location <- uniformLocation program "u_input2d"
  stateInfo <- return $ set uloc location stateInfo
  ebo <- genObjectName
  bindBuffer ElementArrayBuffer $= Just ebo

  texLocation <- uniformLocation program "u_frog"
  stateInfo <- return $ set tloc texLocation stateInfo

  texObject <- genObjectName
  textureBinding Texture2D $= Just texObject
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

  let size = fromIntegral $ length vbuffer * sizeOf (head vbuffer)
  withArray vbuffer $ \ptr -> bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let indicesSize = fromIntegral $ length ibuffer * sizeOf (head ibuffer)
  withArray ibuffer $ \ptr -> bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)

  withArray bitmap $ \ptr ->
    texImage2D
      Texture2D
      NoProxy
      0 -- mipmaps
      RGBA8 -- internal type
      (TextureSize2D (fromIntegral tw) (fromIntegral th))
      0
      (PixelData RGBA UnsignedByte ptr)

  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  depthFunc $= Just Lequal
  currentProgram $= Just program

  print program
  auniforms <- GL.get $ activeUniforms program
  print auniforms

  return stateInfo
