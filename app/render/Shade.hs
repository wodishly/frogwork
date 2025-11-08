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
import Mean (twimap, twin)


drawFaces :: Int32 -> IO ()
drawFaces count = drawElements Triangles count UnsignedInt (bufferOffset 0)

paths :: ShaderProfile -> (FilePath, FilePath)
paths = twimap (printf "%s/%s.glsl" shaderBasePath) . names

asset :: FilePath -> FilePath
asset = printf "%s/%s.frog" assetsBasePath 

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr . fromIntegral

bufferSize :: Storable a => [a] -> GLsizeiptr
bufferSize buffer = fromIntegral (length buffer * (sizeOf.head) buffer)

data Mesh = Mesh {
    _program :: Program
  , _vao :: VertexArrayObject
  , _tex :: Maybe TextureObject
  , _file :: Maybe FrogFile
  , _uniformMap :: UniformMap
  , _elementCount :: Int32
  , _transform :: FrogMatrix
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

setMeshTransform :: Mesh -> FrogMatrix -> IO Mesh
setMeshTransform m t = return m { _transform = t }

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
        Left assetful -> (
            shaderProfile assetful
          , Just (asset $ runIdentity (filePath assetful))
          )
        Right assetless -> (shaderProfile assetless, Nothing)

  p <- uncurry brew (paths sProfile)
  currentProgram $= Just p
  let u = HM.fromList $ map (second (uniformLocation p) . twin) (uniforms sProfile)
  return (Concoction p u path)

begetMeshes :: IO [Mesh]
begetMeshes = do
  froggy <- createAssetMesh defaultAssetMeshProfile
    >>= flip setMeshTransform (fromTranslation [0, 0, 0])

  earth <- makeSimpleMesh defaultSimpleMeshProfile

  farsee <- createAssetMesh (createAsset "tv")
    >>= flip setMeshTransform (fromTranslation [-2, 1, 2])

  return [froggy, earth, farsee]

useMesh :: Mesh -> IO ()
useMesh mesh = do
  currentProgram $= Just (program mesh)
  bindVertexArrayObject $= Just (_vao mesh)
  textureBinding Texture2D $= _tex mesh

drawMesh :: Mesh -> FrogMatrix -> FrogMatrix -> IO ()
drawMesh mesh projectionMatrix viewMatrix = do
  useMesh mesh

  -- the bindings seem to be broken here? :(
  -- projLocation <- uniforms HM.! "u_projection_matrix"
  -- m <- newMatrix ColumnMajor (S.toList projectionMatrix) :: IO (GLmatrix GLfloat)
  -- withMatrix m $ const $ uniformv projLocation 1

  UniformLocation mLocation <- get (uniformMap mesh ! "u_model_matrix")
  S.unsafeWith (flatten $ _transform mesh) (GLRaw.glUniformMatrix4fv mLocation 1 1)

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

  drawFaces (_elementCount mesh)

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
    , _vao = vao
    , _tex = Just texObject
    , _file = Just frogFile
    , _uniformMap = hmap
    , _elementCount = frogFile^.indexCount
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
