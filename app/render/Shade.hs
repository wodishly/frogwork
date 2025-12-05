module Shade (
  module Shade,
  module GL,
  module Foreign,
) where

import Data.Binary.Get
import Data.HashMap.Lazy hiding (map)

import Foreign (Int32, Ptr, Storable (sizeOf), Word8, new, nullPtr, peek, peekArray, plusPtr, withArray, (.>>.))
import Numeric.LinearAlgebra hiding (format, (!))
import Graphics.Rendering.OpenGL as GL hiding (bitmap, flush, get, position, scale, texture)

import qualified Graphics.Rendering.OpenGL as GL (get)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as HM hiding (map)
import qualified Data.Vector.Storable as S
import qualified Graphics.GL as GLRaw

import FastenMain
import FastenShade
import FrogSpell
import Skeleton
import Matrix hiding ((!))
import Mean
import Spell
import Time

import MothSpell as MOTH


data Meshhoard = Meshhoard {
  spitfuls :: [Meshset],
  spitlesses :: [Mesh],
  stavemesh :: Mesh,
  grimes :: Grimes
}

data Meshset = Meshset {
  main :: Mesh,
  hitframe :: Mesh
}

data Grimes = Grimes {
  speechframe :: Mesh,
  others :: [Mesh]
}

uncull :: IO ()
uncull = GLRaw.glDisable GLRaw.GL_CULL_FACE

drawFaces :: Int32 -> IO ()
drawFaces count = drawElements Triangles count UnsignedInt (bufferOffset 0)

paths :: ShaderProfile -> Twain FilePath
paths = twimap (printf "%s/%s.glsl" shaderBasePath) . names

asset :: FilePath -> FilePath
asset = printf "%s/%s.frog" assetsBasePath

bufferOffset :: Int -> Ptr Int
bufferOffset = plusPtr nullPtr . fromIntegral

bufferSize :: Storable a => [a] -> GLsizeiptr
bufferSize = fromIntegral . uncurry (*) . doBoth length (sizeOf . head)

data Mesh = Mesh {
  program :: Program,
  vao :: VertexArrayObject,
  vbo :: BufferObject,
  uvbo :: BufferObject,
  tex :: Maybe TextureObject,
  file :: Maybe FrogFile,
  uniformMap :: UniformMap,
  elementCount :: Int32,
  transform :: FrogMatrix,
  meshAnimation :: Maybe Animation
}

instance Show Mesh where
  show (Mesh a b c d e _ _ h i _) = show a ++ show b ++ show c ++ show d ++ show e ++ show h ++ show i

data Concoction = Concoction {
  program :: Program,
  uniformMap :: UniformMap,
  road :: Maybe String
}

makeAsset :: String -> AssetMeshProfile
makeAsset = AssetMeshProfile

setMeshTransform :: FrogMatrix -> Mesh -> Mesh
setMeshTransform t m = m { transform = t }

-- | Compiles ("kneads") the given kind of shader from the given path.
knead :: ShaderType -> FilePath -> IO Shader
knead kind path = do
  shader <- createShader kind
  (shaderSourceBS shader $=) =<< BS.readFile path

  compileShader shader

  status <- GL.get (compileStatus shader)
  print =<< GL.get (shaderInfoLog shader)
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

  print =<< GL.get (shaderInfoLog vertexShader)
  print =<< GL.get (shaderInfoLog fragmentShader)

  GL.get (linkStatus p) >>= flip unless (error "fuck")
  return p

brewProfile :: MeshProfile -> IO Concoction
brewProfile mProfile = do
  -- parse profile
  let (sProfile, rd) = case mProfile of
        Left assetful@AssetMeshProfile { road } -> (shaderProfile assetful, Just $ asset road)
        Right assetless -> (shaderProfile assetless, Nothing)

  pp <- brew (paths sProfile)
  currentProgram $= Just pp
  let u = HM.fromList $ map (second (uniformLocation pp) . twin) (uniforms sProfile)
  return (Concoction pp u rd)

useMesh :: Mesh -> IO ()
useMesh Mesh { program, vao, tex } = do
  currentProgram $= Just program
  bindVertexArrayObject $= Just vao
  textureBinding Texture2D $= tex

-- idk if i like this, but something like this
allocVector :: Storable a => Mesh -> Vector a -> String -> (GLint -> Ptr a -> IO ()) -> IO ()
allocVector Mesh { uniformMap } prop uniformKey callback =
  whenJust (HM.lookup uniformKey uniformMap) $
    GL.get >=> \(UniformLocation location) -> S.unsafeWith prop (callback location)

uniformMatrix :: GLint -> Ptr GLfloat -> IO ()
uniformMatrix loc = GLRaw.glUniformMatrix4fv loc 1 1

feed :: Uniform u => Mesh -> String -> u -> IO ()
feed mesh name worth = do
  u <- mesh.uniformMap ! name
  uniform u $= worth

drawMesh :: FrogMatrix -> FrogMatrix -> Timewit -> FrogMatrix -> Mesh -> IO ()
drawMesh projectionMatrix orthographicMatrix Timewit { lifetime } viewMatrix mesh@Mesh { uniformMap, meshAnimation } = do
  useMesh mesh

  allocVector mesh (flatten $ transform mesh) "u_model_matrix" uniformMatrix
  -- TODO: move these to a Uniform Buffer Object
  allocVector mesh (flatten projectionMatrix) "u_projection_matrix" uniformMatrix
  allocVector mesh (flatten viewMatrix) "u_view_matrix" uniformMatrix
  allocVector mesh (flatten orthographicMatrix) "u_orthographic_matrix" uniformMatrix

  let now = lifetime

  when (maybe False playing meshAnimation) $
    whenJust meshAnimation $ \animation -> do
      whenJust (HM.lookup "u_bone_matrices" uniformMap) $ \uLoc -> do
        let (skellington, _finished) = continue animation now
        UniformLocation bonesLocation <- GL.get uLoc
        withArray skellington $
          GLRaw.glUniformMatrix4fv bonesLocation (fromIntegral $ boneCount $ aMoth animation) 1

  whenJust (HM.lookup "u_time" uniformMap)
    (\_ -> feed mesh "u_time" now)

  -- whenJust (HM.lookup "u_stricken" uniformMap)
  --   (\_ -> feed mesh "u_stricken" (1::Float))

  whenJust (HM.lookup "u_texture" uniformMap) $ \uLoc -> do
    activeTexture $= TextureUnit 0
    tex0Pointer <- new (TextureUnit 0)
    location <- uLoc
    uniformv location 1 tex0Pointer

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
