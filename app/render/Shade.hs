module Shade where

import Control.Monad (unless)
import Data.Maybe (fromJust)
import Data.Binary.Get (runGet)
import Text.Printf (printf)
import Data.HashMap.Strict (HashMap, (!))

import Foreign (Storable, Int32, Ptr, nullPtr, plusPtr, sizeOf, withArray)
import Foreign.Marshal (new)
import Graphics.Rendering.OpenGL as GL
import qualified Data.HashMap.Lazy as HM (fromList, lookup)
import qualified Graphics.GL as GLRaw
import qualified Data.ByteString as BS (readFile)

import SDL (time)
import Numeric.LinearAlgebra (ident, flatten)
import FastenShade (ShaderProfile (..), AssetMeshProfile (..), MeshProfile, SimpleMeshProfile (..), Shaderful (..), defaultSimpleMeshProfile, defaultAssetMeshProfile)
import File (FrogFile (..), getFrogBytes, parseFrogFile)
import Matrix (FrogMatrix, fromTranslation)
import FastenMain (shaderBasePath, assetsBasePath)
import qualified Data.Vector.Storable as S (unsafeWith)
import Mean (twimap, twin)
import Data.Bifunctor (Bifunctor(second))
import Control.Lens (makeLenses, (^.))

drawFaces :: Int32 -> IO ()
drawFaces count = drawElements Triangles count UnsignedInt (bufferOffset 0)

paths :: ShaderProfile -> (FilePath, FilePath)
paths = twimap (printf "%s/%s.glsl" shaderBasePath) . names

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr . fromIntegral

bufferSize :: Storable a => [a] -> GLsizeiptr
bufferSize buffer = fromIntegral (length buffer * (sizeOf.head) buffer)

type UniformMap = HashMap [Char] (GettableStateVar UniformLocation)

data Mesh = Mesh {
    _program :: Program
  , _vao :: VertexArrayObject
  , _tex :: Maybe TextureObject
  , _file :: Maybe FrogFile
  , _uniformMap :: UniformMap
  , _elementCount :: Int32
  , _transform :: FrogMatrix
}
makeLenses ''Mesh

createAsset :: String -> AssetMeshProfile
createAsset name = AssetMeshProfile {
  aMeshFileName = name
}

setMeshTransform :: Mesh -> FrogMatrix -> IO Mesh
setMeshTransform m t = return m { _transform = t }

data Concoction = Concoction {
    prog :: Program
  , umap :: UniformMap
  , fpath :: Maybe String
}

-- | Compiles ("kneads") a shader of type @t@ from path @path@.
knead :: ShaderType -> FilePath -> IO Shader
knead t path = do
  shader <- createShader t
  BS.readFile path >>= (shaderSourceBS shader $=)

  compileShader shader

  get (compileStatus shader)
    >>= \status -> get (shaderInfoLog shader) >>= print
    >> unless status (error ((if t==VertexShader then "vertex" else "fragment")++" shader failed to compile :("))

  return shader

-- | A boilerplate function to initialize a shader.
-- But `boil` < Lat. `bulliō`, and `well` (< OE `weallan`)
-- is too overloaded, whence `brew` (< OE `brēowan`).
brew :: FilePath -> FilePath -> IO Program
brew vsPath fsPath = do
  -- load sources + compile, attach, and link shaders
  vertexShader <- knead VertexShader vsPath
  fragmentShader <- knead FragmentShader fsPath

  p <- createProgram
  attachShader p vertexShader
  attachShader p fragmentShader
  bindFragDataLocation p "color" $= 0
  linkProgram p
  get (linkStatus p) >>= flip unless (error "fuck")
  return p

brewProfile :: MeshProfile -> IO Concoction
brewProfile mProfile = do
  -- parse profile
  let (sProfile, path) = case mProfile of
        Left assetful -> (shaderProfile assetful, Just $ printf "%s/%s.frog" assetsBasePath (aMeshFileName assetful))
        Right simple -> (shaderProfile simple, Nothing)

  p <- uncurry brew (paths sProfile)
  currentProgram $= Just p
  return Concoction {
      prog = p
    , umap = HM.fromList $ map (second (uniformLocation p) . twin) (uniforms sProfile)
    , fpath = path
  }

begetMeshes :: IO [Mesh]
begetMeshes = do
  froggy <- createAssetMesh defaultAssetMeshProfile
    >>= flip setMeshTransform (fromTranslation [0, -2, -5])

  earth <- makeSimpleMesh defaultSimpleMeshProfile

  farsee <- createAssetMesh (createAsset "tv")
    >>= flip setMeshTransform (fromTranslation [2, -2, -5])

  return [froggy, earth, farsee]

useMesh :: Mesh -> IO ()
useMesh mesh = do
  currentProgram $= Just (mesh^.program)
  bindVertexArrayObject $= Just (mesh^.vao)
  textureBinding Texture2D $= (mesh^.tex)

drawMesh :: Mesh -> FrogMatrix -> FrogMatrix -> IO ()
drawMesh mesh projectionMatrix viewMatrix = do
  let uniforms = mesh^.uniformMap
  useMesh mesh

  -- the bindings seem to be broken here? :(
  -- projLocation <- uniforms HM.! "u_projection_matrix"
  -- m <- newMatrix ColumnMajor (S.toList projectionMatrix) :: IO (GLmatrix GLfloat)
  -- withMatrix m $ const $ uniformv projLocation 1

  (UniformLocation mLocation) <- get (uniforms ! "u_model_matrix")
  S.unsafeWith (flatten $ mesh^.transform) (GLRaw.glUniformMatrix4fv mLocation 1 1)

  -- TODO: move these to a Uniform Buffer Object
  (UniformLocation projLocation) <- get (uniforms ! "u_projection_matrix")
  S.unsafeWith (flatten projectionMatrix) (GLRaw.glUniformMatrix4fv projLocation 1 1)
  (UniformLocation viewLocation) <- get (uniforms ! "u_view_matrix")
  S.unsafeWith (flatten viewMatrix) (GLRaw.glUniformMatrix4fv viewLocation 1 1)

  timeLocation <- uniforms ! "u_time"
  let u = uniform timeLocation :: StateVar GLfloat
  time >>= (u $=)

  case HM.lookup "u_texture" uniforms of
    Just a -> do
      activeTexture $= TextureUnit 0
      tex0Pointer <- new (TextureUnit 0)
      location <- a
      uniformv location 1 tex0Pointer
    _ -> return ()

  drawFaces (mesh^.elementCount)

createAssetMesh :: AssetMeshProfile -> IO Mesh
createAssetMesh mprofile = do
  (Concoction pro hmap filePath) <- brewProfile (Left mprofile)

  -- read all the data
  fbytes <- getFrogBytes (fromJust filePath)
  let frogFile = runGet parseFrogFile fbytes
      vbuffer = positionBuffer frogFile
      nbuffer = normalBuffer frogFile
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

  -- normal attribute
  nbo <- genObjectName
  bindBuffer ArrayBuffer $= Just nbo

  withArray nbuffer $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize nbuffer, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 2)
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 2) $= Enabled

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
      (TextureSize2D (fromIntegral texw) (fromIntegral texh))
      0
      (PixelData RGBA UnsignedByte ptr)

  print pro
  GL.get (activeUniforms pro) >>= print

  -- ✿*,(*´◕ω◕`*)+✿.*
  return Mesh {
      _program = pro
    , _vao = vao
    , _tex = Just texObject
    , _file = Just frogFile
    , _uniformMap = hmap
    , _elementCount = indexCount frogFile
    , _transform = ident 4
  }

makeSimpleMesh :: SimpleMeshProfile -> IO Mesh
makeSimpleMesh profile = do
  Concoction pro hmap _ <- brewProfile (Right profile)

  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo

  let vb = vbuffer profile
  withArray vb $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize vb, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 0)
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  -- index buffer
  let ib = ibuffer profile
  genObjectName >>= (bindBuffer ElementArrayBuffer $=) . Just
  withArray ib $ \ptr ->
    bufferData ElementArrayBuffer $= (bufferSize ib, ptr, StaticDraw)

  return Mesh {
      _program = pro
    , _vao = vao
    , _tex = Nothing
    , _file = Nothing
    , _uniformMap = hmap
    , _elementCount = fromIntegral (length ib)
    , _transform = ident 4
  }
