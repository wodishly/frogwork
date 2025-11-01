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
import Text.Printf

-- data sent to the renderer went requesting a mesh
data MeshProfile = MeshProfile
  {
    -- name of frog file
    meshFileName :: String,
    -- name of vertex shader GLSL file (without extension)
    vertexShaderName :: String,
    -- name of fragment shader GLSL file (without extension)
    fragmentShaderName :: String
  }

defaultMeshProfile :: MeshProfile
defaultMeshProfile = MeshProfile {
  meshFileName = "test",
  vertexShaderName = "vertex",
  fragmentShaderName ="fragment"
}

shaderBasePath :: String
shaderBasePath = "app/shaders"
assetsBasePath :: String
assetsBasePath = "assets"

shade :: StateInfo -> IO StateInfo
shade stateInfo = do
  -- parse profile (TODO: pass in profile as an arg)
  let profile = defaultMeshProfile
  let filePath = printf "%s/%s.frog" assetsBasePath (meshFileName profile) 
  let vertexShaderPath = printf "%s/%s.glsl" shaderBasePath (vertexShaderName profile) 
  let fragmentShaderPath = printf "%s/%s.glsl" shaderBasePath (fragmentShaderName profile) 

  -- read all the data
  fbytes <- getFrogBytes filePath
  let mesh = runGet parseFrogFile fbytes
  let vbuffer = positionBuffer mesh
  let ibuffer = indexBuffer mesh
  let ubuffer = uvBuffer mesh
  let bitmap = bitmapBuffer mesh
  let texw = texWidth mesh
  let texh = texHeight mesh

  -- load sources + compile, attach, and link shaders
  vertexShader <- createShader VertexShader
  vertexSource <- BS.readFile vertexShaderPath
  shaderSourceBS vertexShader $= vertexSource
  compileShader vertexShader
  fragmentShader <- createShader FragmentShader
  fragmentSource <- BS.readFile fragmentShaderPath
  shaderSourceBS fragmentShader $= fragmentSource
  compileShader fragmentShader
  vertexStatus <- get (compileStatus vertexShader)
  fragmentStatus <- get (compileStatus fragmentShader)

  vsLog <- get $ shaderInfoLog vertexShader
  putStrLn vsLog
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

  -- position attribute
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo

  let vbufferSize = fromIntegral $ length vbuffer * sizeOf (head vbuffer)
  withArray vbuffer $ \ptr -> bufferData ArrayBuffer $= (vbufferSize, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 0) 
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  -- uv attribute
  uvbo <- genObjectName
  bindBuffer ArrayBuffer $= Just uvbo

  let ubufferSize = fromIntegral (length ubuffer * sizeOf (head ubuffer))
  withArray ubuffer $ \ptr ->
    bufferData ArrayBuffer $= (ubufferSize, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 1)
    $= (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 1) $= Enabled

  -- index buffer
  ebo <- genObjectName
  bindBuffer ElementArrayBuffer $= Just ebo
  let indicesSize = fromIntegral $ length ibuffer * sizeOf (head ibuffer)
  withArray ibuffer $ \ptr -> bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)

  -- uniform buffers
  location <- uniformLocation program "u_input2d"
  stateInfo <- return $ set uloc location stateInfo

  -- texture uniform
  texLocation <- uniformLocation program "u_frog"
  stateInfo <- return $ set tloc texLocation stateInfo

  texObject <- genObjectName
  textureBinding Texture2D $= Just texObject
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

  withArray bitmap $ \ptr ->
    texImage2D
      Texture2D
      NoProxy
      0 -- mipmaps
      RGBA8 -- internal type
      (TextureSize2D (fromIntegral texw) (fromIntegral texh))
      0
      (PixelData RGBA UnsignedByte ptr)

  -- ✿*,(*´◕ω◕`*)+✿.*
  depthFunc $= Just Lequal
  currentProgram $= Just program

  print program
  auniforms <- GL.get $ activeUniforms program
  print auniforms

  return stateInfo

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr . fromIntegral
