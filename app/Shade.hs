{-# OPTIONS_GHC -Wno-name-shadowing #-}
{- HLINT ignore "Redundant bracket" -}

module Shade where

import Control.Lens
import Control.Monad (unless)
import Data.Binary.Get
import Foreign.Marshal
import Foreign (Storable, sizeOf)
import Text.Printf

import qualified Data.ByteString as BS (readFile)
import Graphics.Rendering.OpenGL as GL

import File
import Light
import State
import Fast
import Rime
import GHC.Word

-- data sent to the renderer went requesting a mesh
data MeshProfile = MeshProfile {
  -- name of frog file
  meshFileName :: String
  -- name of vertex shader GLSL file (without extension)
, vertexShaderName :: String
  -- name of fragment shader GLSL file (without extension)
, fragmentShaderName :: String
}

defaultMeshProfile :: MeshProfile
defaultMeshProfile = MeshProfile {
  meshFileName = "test"
, vertexShaderName = "vertex"
, fragmentShaderName ="fragment"
}

bufferSize :: Storable a => [a] -> GLsizeiptr
bufferSize buffer = fromIntegral (length buffer * (sizeOf.head) buffer)

-- A boilerplate function to initialize a shader.
-- But `boil` < Lat. `bulliō`, and `well` (< OE `weallan`)
-- is too overloaded, whence `brew` (< OE `brēowan`).
brew :: FilePath -> FilePath -> IO Program
brew vertexShaderPath fragmentShaderPath = do
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
  print vsLog
  fsLog <- get $ shaderInfoLog fragmentShader
  print fsLog

  unless vertexStatus (error "vertex shader failed to compile :(")
  unless fragmentStatus (error "fragment shader failed to compile :(")

  program <- createProgram
  attachShader program vertexShader
  attachShader program fragmentShader
  bindFragDataLocation program "color" $= 0
  linkProgram program
  linkStatus <- get (linkStatus program)
  unless linkStatus (error "fuck")

  return program

shade :: StateInfo -> MeshProfile -> IO StateInfo
shade stateInfo profile = do
  -- parse profile
  let filePath = printf "%s/%s.frog" assetsBasePath (meshFileName profile)
      vertexShaderPath = printf "%s/%s.glsl" shaderBasePath (vertexShaderName profile)
      fragmentShaderPath = printf "%s/%s.glsl" shaderBasePath (fragmentShaderName profile)

  -- read all the data
  fbytes <- getFrogBytes filePath
  let mesh = runGet parseFrogFile fbytes
      vbuffer = positionBuffer mesh
      ibuffer = indexBuffer mesh
      ubuffer = uvBuffer mesh
      bitmap = bitmapBuffer mesh
      texw = texWidth mesh
      texh = texHeight mesh

  program <- brew vertexShaderPath fragmentShaderPath

  -- position attribute
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo

  -- bespokeness

  withArray vbuffer $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize vbuffer, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 0)
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  -- uv attribute
  uvbo <- genObjectName
  bindBuffer ArrayBuffer $= Just uvbo

  withArray ubuffer $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize ubuffer, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 1)
    $= (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 1) $= Enabled

  -- index buffer
  ebo <- genObjectName
  bindBuffer ElementArrayBuffer $= Just ebo
  withArray ibuffer $ \ptr ->
    bufferData ElementArrayBuffer $= (bufferSize ibuffer, ptr, StaticDraw)

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
      (TextureSize2D (cast texw) (cast texh))
      0
      (PixelData RGBA UnsignedByte ptr)

  -- ✿*,(*´◕ω◕`*)+✿.*
  depthFunc $= Just Lequal
  currentProgram $= Just program

  print program

  auniforms <- GL.get (activeUniforms program)
  print auniforms

  return $ set programs ((stateInfo^.programs) ++ [(program, vao)]) stateInfo

shadeSheet :: StateInfo -> IO StateInfo
shadeSheet stateInfo = do
  program <- brew "app/shaders/vertex_sheet.glsl" "app/shaders/fragment_sheet.glsl"

  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo

  let vs = [
           Vertex3 (-1) (-0.9) ( 1 :: GLfloat) --SW
         , Vertex3 (-1) (-1.0) ( 1)            --NW
         , Vertex3 ( 1) (-1.0) ( 1)            --NE
         , Vertex3 ( 1) (-0.9) ( 1)            --SE
        ]

  withArray vs $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize vs, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 0)
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  let ibuffer = [
          0, 1, 2 :: Word32
        , 2, 3, 0
        ]

  -- index buffer
  ebo <- genObjectName
  bindBuffer ElementArrayBuffer $= Just ebo
  withArray ibuffer $ \ptr ->
    bufferData ElementArrayBuffer $= (bufferSize ibuffer, ptr, StaticDraw)

  depthFunc $= Just Lequal
  currentProgram $= Just program

  return $ set programs ((stateInfo^.programs) ++ [(program, vao)]) stateInfo
