{-# OPTIONS_GHC -Wno-name-shadowing #-}
{- HLINT ignore "Redundant bracket" -}

module Shade where

import Control.Monad (unless)
import Data.Word (Word32)
import Data.Binary.Get (runGet)
import qualified Data.HashMap.Strict as HM
import Foreign (Storable, sizeOf, withArray)
import Text.Printf (printf)

import qualified Data.ByteString as BS (readFile)
import Graphics.Rendering.OpenGL as GL

import File

import Fast
import Rime
import Foreign.Marshal (new)
import Data.Maybe (fromJust)
import Light 
import Data.Int

-- data sent to the renderer when requesting a mesh
data AssetMeshProfile = AssetMeshProfile {
  aMeshFileName :: String,
  aShaderProfile :: ShaderProfile
}

data SimpleMeshProfile = SimpleMeshProfile {
  sVertexBuffer :: [Vertex3 GLfloat],
  sIndexBuffer :: [Word32],
  sShaderProfile :: ShaderProfile
}

data ShaderProfile = ShaderProfile {
  -- name of vertex shader GLSL file (without extension)
  vertexShaderName :: String,
  -- name of fragment shader GLSL file (without extension)
  fragmentShaderName :: String,
  -- uniform symbol names
  uniforms :: [String]
}

data Mesh = Mesh {
  program :: Program,
  vao :: VertexArrayObject,
  tex :: Maybe TextureObject,
  file :: Maybe FrogFile,
  uniformMap :: UniformMap,
  elementCount :: Int32
}
type UniformMap = HM.HashMap [Char] (GettableStateVar UniformLocation)

defaultAssetMeshProfile :: AssetMeshProfile
defaultAssetMeshProfile = AssetMeshProfile {
  aMeshFileName = "test",
  aShaderProfile = defaultAssetShaderProfile
}

defaultAssetShaderProfile :: ShaderProfile
defaultAssetShaderProfile = ShaderProfile {
  vertexShaderName = "vertex",
  fragmentShaderName = "texture_fragment",
  uniforms = ["u_texture", "u_input2d"]
}

createAsset :: String -> AssetMeshProfile
createAsset name = AssetMeshProfile { 
  aMeshFileName = name,
  aShaderProfile = defaultAssetShaderProfile
}

defaultSimpleMeshProfile :: SimpleMeshProfile
defaultSimpleMeshProfile = SimpleMeshProfile {
  sVertexBuffer = floorVbuffer,
  sIndexBuffer = floorIbuffer,
  sShaderProfile = defaultSimpleShaderProfile
}

defaultSimpleShaderProfile :: ShaderProfile
defaultSimpleShaderProfile = ShaderProfile {
  vertexShaderName = "vertex_sheet",
  fragmentShaderName = "color_fragment",
  uniforms = []
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

data Concoction = Concoction {
  prog :: Program,
  umap :: UniformMap,
  fpath :: Maybe String
}

type MeshProfile = Either AssetMeshProfile SimpleMeshProfile
brewProfile :: MeshProfile -> IO Concoction
brewProfile mprofile = do
  -- parse profile
  let (sprofile, filePath) = case mprofile of
        Left a -> (aShaderProfile a, Just $ printf "%s/%s.frog" assetsBasePath (aMeshFileName a))
        Right a -> (sShaderProfile a, Nothing)
  let uniformNames = uniforms sprofile
  let vertexShaderPath = printf "%s/%s.glsl" shaderBasePath (vertexShaderName sprofile)
      fragmentShaderPath = printf "%s/%s.glsl" shaderBasePath (fragmentShaderName sprofile)
  program <- brew vertexShaderPath fragmentShaderPath
  let hmap = HM.fromList $ map (\u -> (u, uniformLocation program u)) uniformNames
  return Concoction {
    prog = program,
    umap = hmap,
    fpath = filePath
  }

useMesh :: Mesh -> IO ()
useMesh (Mesh program vao tex _ _ _) = do
  currentProgram $= Just program
  bindVertexArrayObject $= Just vao
  textureBinding Texture2D $= tex

drawMesh :: Mesh -> IO ()
drawMesh mesh = do
  let uniforms = uniformMap mesh
  let count = elementCount mesh
  useMesh mesh

  let tex0Location = HM.lookup "u_texture" uniforms
  case tex0Location of
    Just a -> do
      activeTexture $= TextureUnit 0
      tex0Pointer <- new (TextureUnit 0)
      location <- a
      uniformv location 1 tex0Pointer
    _ -> return ()

  drawFaces count

createAssetMesh :: AssetMeshProfile -> IO Mesh
createAssetMesh mprofile = do
  (Concoction program hmap filePath) <- brewProfile (Left mprofile)

  -- read all the data
  fbytes <- getFrogBytes (fromJust filePath)
  let frogFile = runGet parseFrogFile fbytes
      vbuffer = positionBuffer frogFile
      ibuffer = indexBuffer frogFile
      ubuffer = uvBuffer frogFile
      bitmap = bitmapBuffer frogFile
      texw = texWidth frogFile
      texh = texHeight frogFile


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

  -- texture uniform
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

  print program
  auniforms <- GL.get (activeUniforms program)
  print auniforms

  -- ✿*,(*´◕ω◕`*)+✿.*
  return Mesh {
    program = program,
    vao = vao,
    tex = Just texObject,
    file = Just frogFile,
    uniformMap = hmap,
    elementCount = indexCount frogFile
  }

createSimpleMesh :: SimpleMeshProfile -> IO Mesh
createSimpleMesh mprofile = do
  (Concoction program hmap _) <- brewProfile (Right mprofile)

  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo

  let vbuffer = sVertexBuffer mprofile
  let ibuffer = sIndexBuffer mprofile

  withArray vbuffer $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize vbuffer, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 0)
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  -- index buffer
  ebo <- genObjectName
  bindBuffer ElementArrayBuffer $= Just ebo
  withArray ibuffer $ \ptr ->
    bufferData ElementArrayBuffer $= (bufferSize ibuffer, ptr, StaticDraw)

  return Mesh {
    program = program,
    vao = vao,
    tex = Nothing,
    file = Nothing,
    uniformMap = hmap,
    elementCount = fromIntegral (length ibuffer)
  }


floorVbuffer :: [Vertex3 GLfloat]
floorVbuffer = [
          Vertex3 (-1) (-0.9) ( 1.0)            --SW
        , Vertex3 (-1) (-1.0) ( 1.0)            --NW
        , Vertex3 ( 1) (-1.0) ( 1.0)            --NE
        , Vertex3 ( 1) (-0.9) ( 1.0)            --SE
      ]

floorIbuffer :: [Word32]
floorIbuffer = [
        0, 1, 2
      , 2, 3, 0
      ]
