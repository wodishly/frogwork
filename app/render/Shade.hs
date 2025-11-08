module Shade (
  Mesh
, begetMeshes
, drawMesh
, setMeshTransform
) where

import Control.Lens ((^.))
import Control.Monad (unless)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (Bifunctor (second))
import Data.Binary.Get (runGet)
import Data.HashMap.Lazy ((!))
import Data.Maybe (fromJust)
import Numeric.LinearAlgebra (flatten, ident)
import Text.Printf (printf)

import Foreign (Int32, Ptr, Storable, new, nullPtr, plusPtr, sizeOf, withArray)
import Graphics.Rendering.OpenGL as GL
import SDL (time)

import qualified Data.ByteString as BS (readFile)
import qualified Data.HashMap.Lazy as HM (fromList, lookup)
import qualified Data.Vector.Storable as S (unsafeWith)
import qualified Graphics.GL as GLRaw (glUniformMatrix4fv)

import FastenMain (assetsBasePath, shaderBasePath)
import FastenShade
import File
import Matrix (FrogMatrix, fromTranslation)
import Mean (Twain, twimap, twin)


drawFaces :: Int32 -> IO ()
drawFaces count = drawElements Triangles count UnsignedInt (bufferOffset 0)

paths :: ShaderProfile -> Twain FilePath
paths = twimap (printf "%s/%s.glsl" shaderBasePath) . names

asset :: FilePath -> FilePath
asset = printf "%s/%s.frog" assetsBasePath

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr . fromIntegral

bufferSize :: Storable a => [a] -> GLsizeiptr
bufferSize buffer = fromIntegral (length buffer * (sizeOf.head) buffer)

data Mesh = Mesh {
    _program :: Program
  , vao :: VertexArrayObject
  , tex :: Maybe TextureObject
  , file :: Maybe FrogFile
  , _uniformMap :: UniformMap
  , elementCount :: Int32
  , transform :: FrogMatrix
}

instance Programful Mesh where
  program = _program
  uniformMap (Mesh _ _ _ _ u _ _) = u

data Concoction = Concoction Program UniformMap (Maybe String)

instance Programful Concoction where
  program (Concoction p _ _) = p
  uniformMap (Concoction _ u _) = u

instance Pathlikeful Maybe Concoction where
  filePath (Concoction _ _ p) = p

makeAsset :: String -> AssetMeshProfile
makeAsset = AssetMeshProfile

setMeshTransform :: FrogMatrix -> Mesh -> IO Mesh
setMeshTransform t m = return m { transform = t }

-- | Compiles ("kneads") the given kind of shader from the given path.
knead :: ShaderType -> FilePath -> IO Shader
knead kind path = do
  shader <- createShader kind
  BS.readFile path >>= (shaderSourceBS shader $=)

  compileShader shader

  status <- get (compileStatus shader)
  get (shaderInfoLog shader) >>= print
  unless status (error $ (case kind of
      VertexShader -> "vertex"
      FragmentShader -> "fragment"
      _ -> error "tfw the error throws an error"
    ) ++ " shader failed to compile :(")

  return shader

-- | A boilerplate function to initialize a shader.
-- But `boil` < Lat. `bulliō`, and `well` (< OE `weallan`)
-- is too overloaded, whence `brew` (< OE `brēowan`).
brew :: Twain FilePath -> IO Program
brew (vsPath, fsPath) = do
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
        Left assetful -> (shaderProfile assetful, Just . asset . runIdentity . filePath $ assetful)
        Right assetless -> (shaderProfile assetless, Nothing)

  p <- brew (paths sProfile)
  currentProgram $= Just p
  let u = HM.fromList $ map (second (uniformLocation p) . twin) (uniforms sProfile)
  return (Concoction p u path)

begetMeshes :: IO [Mesh]
begetMeshes = do
  froggy <- makeAssetMesh defaultAssetMeshProfile
    >>= setMeshTransform (fromTranslation [0, 0, 0])

  earth <- makeSimpleMesh defaultSimpleMeshProfile

  farsee <- makeAssetMesh (makeAsset "tv")
    >>= setMeshTransform (fromTranslation [-2, 1, 2])

  return [froggy, earth, farsee]

useMesh :: Mesh -> IO ()
useMesh mesh = do
  currentProgram $= Just (program mesh)
  bindVertexArrayObject $= Just (vao mesh)
  textureBinding Texture2D $= tex mesh

drawMesh :: FrogMatrix -> FrogMatrix -> Mesh -> IO ()
drawMesh projectionMatrix viewMatrix mesh = do
  useMesh mesh

  -- the bindings seem to be broken here? :(
  -- projLocation <- uniforms HM.! "u_projection_matrix"
  -- m <- newMatrix ColumnMajor (S.toList projectionMatrix) :: IO (GLmatrix GLfloat)
  -- withMatrix m $ const $ uniformv projLocation 1

  UniformLocation mLocation <- get (uniformMap mesh ! "u_model_matrix")
  S.unsafeWith (flatten $ transform mesh) (GLRaw.glUniformMatrix4fv mLocation 1 1)

  -- TODO: move these to a Uniform Buffer Object
  UniformLocation projLocation <- get (uniformMap mesh ! "u_projection_matrix")
  S.unsafeWith (flatten projectionMatrix) (GLRaw.glUniformMatrix4fv projLocation 1 1)
  UniformLocation viewLocation <- get (uniformMap mesh ! "u_view_matrix")
  S.unsafeWith (flatten viewMatrix) (GLRaw.glUniformMatrix4fv viewLocation 1 1)

  timeLocation <- uniformMap mesh ! "u_time"
  time >>= ((uniform timeLocation :: StateVar GLfloat) $=)

  case HM.lookup "u_texture" (uniformMap mesh) of
    Just a -> do
      activeTexture $= TextureUnit 0
      tex0Pointer <- new (TextureUnit 0)
      location <- a
      uniformv location 1 tex0Pointer
    _ -> return ()

  drawFaces (elementCount mesh)

makeAssetMesh :: AssetMeshProfile -> IO Mesh
makeAssetMesh mprofile = do
  (Concoction pro hmap path) <- brewProfile (Left mprofile)

  -- read all the data
  fbytes <- getFrogBytes (fromJust path)
  let frogFile = runGet parseFrogFile fbytes

  -- position attribute
  vao <- genObjectName
  bindVertexArrayObject $= Just vao

  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo

  -- bespokeness

  withArray (frogFile^.positionBuffer) $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize (frogFile^.positionBuffer), ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 0)
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  -- uv attribute
  uvbo <- genObjectName
  bindBuffer ArrayBuffer $= Just uvbo

  withArray (frogFile^.uvBuffer) $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize (frogFile^.uvBuffer), ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 1)
    $= (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 1) $= Enabled

  -- normal attribute
  nbo <- genObjectName
  bindBuffer ArrayBuffer $= Just nbo

  withArray (frogFile^.normalBuffer) $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize (frogFile^.normalBuffer), ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 2)
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 2) $= Enabled

  -- index buffer
  ebo <- genObjectName
  bindBuffer ElementArrayBuffer $= Just ebo
  withArray (frogFile^.indexBuffer) $ \ptr ->
    bufferData ElementArrayBuffer $= (bufferSize (frogFile^.indexBuffer), ptr, StaticDraw)

  -- texture uniform
  texObject <- genObjectName
  textureBinding Texture2D $= Just texObject
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

  withArray (frogFile^.bitmapBuffer) $ \ptr ->
    texImage2D
      Texture2D
      NoProxy
      0 -- mipmaps
      RGBA8 -- internal type
      (uncurry TextureSize2D (twimap fromIntegral $ frogFile^.texSize))
      0
      (PixelData RGBA UnsignedByte ptr)

  print pro
  GL.get (activeUniforms pro) >>= print

  -- ✿*,(*´◕ω◕`*)+✿.*
  return Mesh {
      _program = pro
    , vao = vao
    , tex = Just texObject
    , file = Just frogFile
    , _uniformMap = hmap
    , elementCount = frogFile^.indexCount
    , transform = ident 4
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
    , vao = vao
    , tex = Nothing
    , file = Nothing
    , _uniformMap = hmap
    , elementCount = fromIntegral (length ib)
    , transform = ident 4
  }
