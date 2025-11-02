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
import qualified Graphics.GL as GLRaw
import qualified Data.Vector.Storable as S

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
  aShaderProfile :: ShaderProfile,
  aTransform :: Transform
}

data SimpleMeshProfile = SimpleMeshProfile {
  sVertexBuffer :: [Vertex3 GLfloat],
  sIndexBuffer :: [Word32],
  sShaderProfile :: ShaderProfile,
  sTransform :: Transform
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
  elementCount :: Int32,
  transform :: Transform
}
type UniformMap = HM.HashMap [Char] (GettableStateVar UniformLocation)

defaultAssetMeshProfile :: AssetMeshProfile
defaultAssetMeshProfile = AssetMeshProfile {
  aMeshFileName = "test",
  aShaderProfile = defaultAssetShaderProfile,
  aTransform = identity
}

defaultAssetShaderProfile :: ShaderProfile
defaultAssetShaderProfile = ShaderProfile {
  vertexShaderName = "vertex",
  fragmentShaderName = "texture_fragment",
  uniforms = ["u_projection_matrix", "u_modelview_matrix", "u_texture", "u_input2d"]
}

createAsset :: String -> AssetMeshProfile
createAsset name = AssetMeshProfile {
  aMeshFileName = name,
  aShaderProfile = defaultAssetShaderProfile,
  aTransform = identity
}

setMeshTransform :: Mesh -> Transform -> IO Mesh
setMeshTransform m transform = do
    return $ m { transform = transform }

defaultSimpleMeshProfile :: SimpleMeshProfile
defaultSimpleMeshProfile = SimpleMeshProfile {
  sVertexBuffer = floorVbuffer,
  sIndexBuffer = floorIbuffer,
  sShaderProfile = defaultSimpleShaderProfile,
  sTransform = identity
}

defaultSimpleShaderProfile :: ShaderProfile
defaultSimpleShaderProfile = ShaderProfile {
  vertexShaderName = "vertex_sheet",
  fragmentShaderName = "color_fragment",
  uniforms = ["u_projection_matrix", "u_modelview_matrix"]
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
  currentProgram $= Just program
  let hmap = HM.fromList $ map (\u -> (u, uniformLocation program u)) uniformNames
  return Concoction {
    prog = program,
    umap = hmap,
    fpath = filePath
  }

useMesh :: Mesh -> IO ()
useMesh (Mesh program vao tex _ _ _ _) = do
  currentProgram $= Just program
  bindVertexArrayObject $= Just vao
  textureBinding Texture2D $= tex

type Transform = S.Vector GLfloat
drawMesh :: Mesh -> Transform -> IO ()
drawMesh mesh projectionMatrix = do
  let uniforms = uniformMap mesh
  let count = elementCount mesh
  useMesh mesh

  -- the bindings seem to be broken here? :(
  -- projLocation <- uniforms HM.! "u_projection_matrix"
  -- m <- newMatrix ColumnMajor (S.toList projectionMatrix) :: IO (GLmatrix GLfloat)
  -- withMatrix m $ const $ uniformv projLocation 1

  (UniformLocation projLocation) <- get (uniforms HM.! "u_projection_matrix")
  S.unsafeWith projectionMatrix (GLRaw.glUniformMatrix4fv projLocation 1 1)

  (UniformLocation mvLocation) <- get (uniforms HM.! "u_modelview_matrix")
  S.unsafeWith (transform mesh) (GLRaw.glUniformMatrix4fv mvLocation 1 1)

  let tex0Location = HM.lookup "u_texture" uniforms
  case tex0Location of
    Just a -> do
      activeTexture $= TextureUnit 0
      tex0Pointer <- new (TextureUnit 0)
      location <- a
      uniformv location 1 tex0Pointer
    _ -> return ()

  drawFaces count

data RenderView = RenderView {
  aspect :: Float,
  fov :: Float,
  near :: Float,
  far :: Float
}

identity :: Transform
identity = S.fromList
           [1,0,0,0,
            0,1,0,0,
            0,0,1,0,
            0,0,0,1]
fromTranslation :: GLfloat -> GLfloat -> GLfloat -> Transform
fromTranslation x y z = S.fromList
           [1,0,0,x,
            0,1,0,y,
            0,0,1,z,
            0,0,0,1]

matrix :: [Float] -> Transform
matrix = S.fromList

getProjectionMatrix :: RenderView -> Transform
getProjectionMatrix (RenderView aspect fov near far)
  = S.fromList [1 / (aspect * tan (fov / 2)), 0.0, 0.0, 0.0,
    0.0, 1 / tan (fov / 2), 0.0, 0.0,
    0.0, 0.0,- ((far + near) / (far - near)),- (2.0 * far * near / (far - near)),
    0.0, 0.0, -1.0, 0.0]

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
    elementCount = indexCount frogFile,
    transform = identity
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
    elementCount = fromIntegral (length ibuffer),
    transform = identity
  }

floorVbuffer :: [Vertex3 GLfloat]
floorVbuffer = [
          Vertex3 (-1) (-0) ( 1.0)            --SW
        , Vertex3 (-1) (-1.0) ( 1.0)            --NW
        , Vertex3 ( 1) (-1.0) ( 1.0)            --NE
        , Vertex3 ( 1) (-0) ( 1.0)            --SE
      ]

floorIbuffer :: [Word32]
floorIbuffer = [
        0, 1, 2
      , 2, 3, 0
      ]
