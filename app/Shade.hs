{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Shade where

import Data.ByteString as BS (readFile)
import Foreign (Ptr, nullPtr, plusPtr, sizeOf)
import Control.Monad (unless)
import Graphics.Rendering.OpenGL as GL
import State
import Control.Lens ((^.))
import Control.Lens.Combinators (set)
import Frog (make3dFrogIndices, make3dFrogBitmap, make3dFrogUvs)
import Foreign.Marshal

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr . fromIntegral

shade :: StateInfo -> IO StateInfo
shade stateInfo = do
  let vertices = stateInfo^.frog
  let indices = make3dFrogIndices

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
  fsLog <- get $ shaderInfoLog fragmentShader
  putStrLn fsLog

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

  let uvs = make3dFrogUvs
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

  let size = fromIntegral $ length vertices * sizeOf (head vertices)
  withArray vertices $ \ptr -> bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  let indicesSize = fromIntegral $ length indices * sizeOf (head indices)
  withArray indices $ \ptr -> bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)

  let bitmap = make3dFrogBitmap
  withArray bitmap $ \ptr ->
    texImage2D
      Texture2D
      NoProxy
      0 -- mipmaps
      RGBA8 -- internal type
      (TextureSize2D 128 128)
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


