module Shade (
  Mesh (..)
, bufferSize
, drawMesh
, drawFaces
, setMeshTransform
, makeAsset
, useMesh
, makeAssetMesh
, makeSimpleMesh
, uploadTexture
, Animation (..)
) where

import Control.Monad (unless)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Bifunctor (Bifunctor (second))
import Data.Binary.Get (runGet)
import Data.HashMap.Lazy ((!))
import Data.Maybe (fromJust)
import Numeric.LinearAlgebra (flatten)
import Text.Printf (printf)

import Foreign (Int32, Ptr, Storable, new, nullPtr, plusPtr, sizeOf, withArray, Word8)
import Graphics.Rendering.OpenGL as GL

import qualified Data.ByteString as BS (readFile)
import qualified Data.HashMap.Lazy as HM (fromList, lookup)
import qualified Data.Vector.Storable as S (unsafeWith)
import qualified Graphics.GL as GLRaw (glUniformMatrix4fv)

import FastenMain (assetsBasePath, shaderBasePath)
import FastenShade
import Matrix (FrogMatrix)
import Mean (Twain, twimap, twin, doBoth)
import FrogSpell
import MothSpell as MOTH
import Spell (summon)
import Time
import Numeric.LinearAlgebra.HMatrix ( ident )
import Skeleton (Animation (..), continue)
import Data.Vector.Storable (Vector)

drawFaces :: Int32 -> IO ()
drawFaces count = drawElements Triangles count UnsignedInt (bufferOffset 0)

paths :: ShaderProfile -> Twain FilePath
paths = twimap (printf "%s/%s.glsl" shaderBasePath) . names

asset :: FilePath -> FilePath
asset = printf "%s/%s.frog" assetsBasePath

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr . fromIntegral

bufferSize :: Storable a => [a] -> GLsizeiptr
bufferSize = fromIntegral . uncurry (*) . doBoth length (sizeOf.head)

data Mesh = Mesh {
    _program :: Program
  , vao :: VertexArrayObject
  , vbo :: BufferObject
  , uvbo :: BufferObject
  , tex :: Maybe TextureObject
  , _file :: Maybe FrogFile
  , _uniformMap :: UniformMap
  , elementCount :: Int32
  , transform :: FrogMatrix
  , meshAnimation :: Maybe Animation
}

instance Programful Mesh where
  program = _program
  uniformMap (Mesh _ _ _ _ _ _ x _ _ _) = x

data Concoction = Concoction Program UniformMap (Maybe String)

instance Programful Concoction where
  program (Concoction x _ _) = x
  uniformMap (Concoction _ x _) = x

instance Pathlikeful Maybe Concoction where
  frogFilePath (Concoction _ _ x) = x

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

  get (shaderInfoLog vertexShader) >>= print
  get (shaderInfoLog fragmentShader) >>= print

  get (linkStatus p) >>= flip unless (error "fuck")
  return p

brewProfile :: MeshProfile -> IO Concoction
brewProfile mProfile = do
  -- parse profile
  let (sProfile, path) = case mProfile of
        Left assetful -> (shaderProfile assetful, Just . asset . runIdentity . frogFilePath $ assetful)
        Right assetless -> (shaderProfile assetless, Nothing)

  p <- brew (paths sProfile)
  currentProgram $= Just p
  let u = HM.fromList $ map (second (uniformLocation p) . twin) (uniforms sProfile)
  return (Concoction p u path)

useMesh :: Mesh -> IO ()
useMesh mesh = do
  currentProgram $= Just (program mesh)
  bindVertexArrayObject $= Just (vao mesh)
  textureBinding Texture2D $= tex mesh

-- idk if i like this, but something like this
allocVector :: Storable a => Mesh -> Vector a -> String -> (GLint -> (Ptr a -> IO ())) -> IO ()
allocVector mesh prop uniformKey callback = case HM.lookup uniformKey (uniformMap mesh) of
    Just uLoc -> do
      UniformLocation location <- get uLoc
      _ <- S.unsafeWith prop (callback location)
      return ()
    _ -> return ()

uniformMatrix :: GLint -> Ptr GLfloat -> IO ()
uniformMatrix loc = GLRaw.glUniformMatrix4fv loc 1 1

drawMesh :: FrogMatrix -> FrogMatrix -> FrogMatrix -> Time -> Mesh -> IO ()
drawMesh projectionMatrix viewMatrix orthographicMatrix time mesh = do
  useMesh mesh

  allocVector mesh (flatten $ transform mesh) "u_model_matrix" uniformMatrix
  -- TODO: move these to a Uniform Buffer Object
  allocVector mesh (flatten projectionMatrix) "u_projection_matrix" uniformMatrix
  allocVector mesh (flatten viewMatrix) "u_view_matrix" uniformMatrix
  allocVector mesh (flatten orthographicMatrix) "u_orthographic_matrix" uniformMatrix

  let now = (fromIntegral (lifetime time) / 1000) :: Float
      meshmoth = meshAnimation mesh
  case (meshmoth, maybe False playing meshmoth) of
    (Just animation, True) -> do
      (skellington, _finished) <- continue animation now
      case HM.lookup "u_bone_matrices" (uniformMap mesh) of
        Just uLoc -> do
          UniformLocation bonesLocation <- get uLoc
          withArray skellington $
            GLRaw.glUniformMatrix4fv bonesLocation (fromIntegral $ boneCount $ aMoth animation) 1
        _ -> return ()
    _ -> return ()

  case HM.lookup "u_time" (uniformMap mesh) of
    Just _ -> do
      timeLocation <- uniformMap mesh ! "u_time"
      (uniform timeLocation :: StateVar GLfloat) $= now
    _ -> return ()

  case HM.lookup "u_texture" (uniformMap mesh) of
    Just uLoc -> do
      activeTexture $= TextureUnit 0
      tex0Pointer <- new (TextureUnit 0)
      location <- uLoc
      uniformv location 1 tex0Pointer
    _ -> return ()

  drawFaces (elementCount mesh)


makeAssetMesh :: AssetMeshProfile -> IO Mesh
makeAssetMesh mprofile = do
  (Concoction pro hmap path) <- brewProfile (Left mprofile)

  -- read all the data
  bytes <- summon (fromJust path)
  let frogFile = runGet frogify bytes

  -- position attribute
  vao' <- genObjectName
  bindVertexArrayObject $= Just vao'

  vbo' <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo'

  -- bespokeness

  withArray (positionBuffer frogFile) $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize (positionBuffer frogFile), ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 0)
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  -- uv attribute
  uvbo' <- genObjectName
  bindBuffer ArrayBuffer $= Just uvbo'

  withArray (uvBuffer frogFile) $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize (uvBuffer frogFile), ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 1)
    $= (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 1) $= Enabled

  -- normal attribute
  nbo <- genObjectName
  bindBuffer ArrayBuffer $= Just nbo

  withArray (normalBuffer frogFile) $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize (normalBuffer frogFile), ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 2)
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 2) $= Enabled

  -- bone attributes
  case boneInfluence frogFile of
    4 -> do
      sbo <- genObjectName
      bindBuffer ArrayBuffer $= Just sbo

      withArray (boneBuffer frogFile) $ \ptr ->
        bufferData ArrayBuffer $= (bufferSize (boneBuffer frogFile), ptr, StaticDraw)

      vertexAttribPointer (AttribLocation 3)
        $= (KeepIntegral, VertexArrayDescriptor 4 Int 0 (bufferOffset 0))
      vertexAttribArray (AttribLocation 3) $= Enabled

      wbo <- genObjectName
      bindBuffer ArrayBuffer $= Just wbo

      withArray (weightBuffer frogFile) $ \ptr ->
        bufferData ArrayBuffer $= (bufferSize (weightBuffer frogFile), ptr, StaticDraw)

      vertexAttribPointer (AttribLocation 4)
        $= (ToFloat, VertexArrayDescriptor 4 Float 0 (bufferOffset 0))
      vertexAttribArray (AttribLocation 4) $= Enabled


    _ -> print ("tfw no 🅱ones" :: String) 

  -- index buffer
  ebo <- genObjectName
  bindBuffer ElementArrayBuffer $= Just ebo
  withArray (indexBuffer frogFile) $ \ptr ->
    bufferData ElementArrayBuffer $= (bufferSize (indexBuffer frogFile), ptr, StaticDraw)

  -- texture uniform
  texy <- withArray (bitmapBuffer frogFile)
    (uploadTexture RGBA (twimap fromIntegral $ texSize frogFile))

  print pro
  GL.get (activeUniforms pro) >>= print

  -- ✿*,(*´◕ω◕`*)+✿.*
  return $ Mesh
    pro
    vao'
    vbo'
    uvbo'
    (Just texy)
    (Just frogFile)
    hmap
    (indexCount frogFile)
    (ident 4)
    Nothing

uploadTexture :: PixelFormat -> (GLsizei, GLsizei) -> Ptr Word8 -> IO TextureObject
uploadTexture format (w, h) pointer = do
  thingy <- genObjectName
  textureBinding Texture2D $= Just thingy
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

  texImage2D
    Texture2D
    NoProxy
    0 -- mipmaps
    (case format of
      Red -> R8
      RGBA -> RGBA8
      _ -> error "bad pixel format") -- internal type
    (TextureSize2D w h)
    0
    (PixelData format UnsignedByte pointer)

  return thingy

makeSimpleMesh :: SimpleMeshProfile -> IO Mesh
makeSimpleMesh profile = do
  Concoction pro hmap _ <- brewProfile (Right profile)

  vao' <- genObjectName
  bindVertexArrayObject $= Just vao'

  -- vertex buffer
  vbo' <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo'

  let vb = vbuffer profile
  withArray vb $ \ptr ->
    bufferData ArrayBuffer $= (bufferSize vb, ptr, StaticDraw)

  vertexAttribPointer (AttribLocation 0)
    $= (ToFloat, VertexArrayDescriptor 3 Float 0 (bufferOffset 0))
  vertexAttribArray (AttribLocation 0) $= Enabled

  -- uv attribute
  uvbo' <- genObjectName

  let uvb = uvbuffer profile
  case uvb of
    Just buffer -> do
      bindBuffer ArrayBuffer $= Just uvbo'

      withArray buffer $ \ptr ->
        bufferData ArrayBuffer $= (bufferSize buffer, ptr, StaticDraw)

      vertexAttribPointer (AttribLocation 1)
        $= (ToFloat, VertexArrayDescriptor 2 Float 0 (bufferOffset 0))
      vertexAttribArray (AttribLocation 1) $= Enabled
    Nothing -> return ()

  -- index buffer
  let ib = ibuffer profile
  genObjectName >>= (bindBuffer ElementArrayBuffer $=) . Just
  withArray ib $ \ptr ->
    bufferData ElementArrayBuffer $= (bufferSize ib, ptr, StaticDraw)

  return $ Mesh
    pro
    vao'
    vbo'
    uvbo'
    (texObject profile)
    Nothing
    hmap
    (fromIntegral $ length ib)
    (ident 4)
    Nothing
