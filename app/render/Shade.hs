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
import Data.Maybe (fromJust, fromMaybe)
import Numeric.LinearAlgebra (flatten, toList)
import Text.Printf (printf)

import Foreign (Int32, Ptr, Storable, new, nullPtr, plusPtr, sizeOf, withArray, Word8)
import Graphics.Rendering.OpenGL as GL

import qualified Data.ByteString as BS (readFile)
import qualified Data.HashMap.Lazy as HM (fromList, lookup)
import qualified Data.Vector.Storable as S (unsafeWith)
import qualified Graphics.GL as GLRaw (glUniformMatrix4fv)

import FastenMain (assetsBasePath, shaderBasePath)
import FastenShade
import Matrix (FrogMatrix, toFrogList, FrogVertex (toFrogVector), fromAffine)
import Mean (Twain, twimap, twin, doBoth)
import FrogSpell
import MothSpell as MOTH
import Spell (summon)
import Time
import Data.List (findIndex)
import Rime (clamp, (<->), (<+>), Point4, (^/), Point3)
import Numeric.LinearAlgebra.HMatrix ( ident, dot, (><) )
import Data.Fixed (mod')

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

drawMesh :: FrogMatrix -> FrogMatrix -> FrogMatrix -> Time -> Mesh -> IO ()
drawMesh projectionMatrix viewMatrix orthographicMatrix time mesh = do
  useMesh mesh

  -- the bindings seem to be broken here? :(
  -- projLocation <- uniforms HM.! "u_projection_matrix"
  -- m <- newMatrix ColumnMajor (S.toList projectionMatrix) :: IO (GLmatrix GLfloat)
  -- withMatrix m $ const $ uniformv projLocation 1

  case HM.lookup "u_model_matrix" (uniformMap mesh) of
    Just uLoc -> do
      UniformLocation mLocation <- get uLoc
      S.unsafeWith (flatten $ transform mesh) (GLRaw.glUniformMatrix4fv mLocation 1 1)
    _ -> return ()

  -- -- TODO: move these to a Uniform Buffer Object
  case HM.lookup "u_projection_matrix" (uniformMap mesh) of
    Just uLoc -> do
      UniformLocation projLocation <- get uLoc
      S.unsafeWith (flatten projectionMatrix) (GLRaw.glUniformMatrix4fv projLocation 1 1)
    _ -> return ()

  case HM.lookup "u_orthographic_matrix" (uniformMap mesh) of
    Just uLoc -> do
      UniformLocation orthLocation <- get uLoc
      S.unsafeWith (flatten orthographicMatrix) (GLRaw.glUniformMatrix4fv orthLocation 1 1)
    _ -> return ()

  case HM.lookup "u_view_matrix" (uniformMap mesh) of
    Just uLoc -> do
      UniformLocation viewLocation <- get uLoc
      S.unsafeWith (flatten viewMatrix) (GLRaw.glUniformMatrix4fv viewLocation 1 1)
    _ -> return ()

  let now = (fromIntegral (lifetime time) / 1000) :: Float
  case meshAnimation mesh of
    Just animation -> do
      skellington <- play animation now
      case HM.lookup "u_bone_matrices" (uniformMap mesh) of
        Just uLoc -> do
          UniformLocation bonesLocation <- get uLoc
          withArray skellington $
            \ptr -> GLRaw.glUniformMatrix4fv bonesLocation (fromIntegral $ boneCount $ aMoth animation) 1 ptr
        _ -> return ()
    Nothing -> return ()

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

data Animation = Animation {
  aMoth :: MothFile
, aTime :: Float
}

(*^) :: (Functor f, Num a) => a -> f a -> f a
(*^) a = fmap (a*)
{-# INLINE (*^) #-}
slerp :: Point4 -> Point4 -> Float -> Point4
slerp (Vertex4 qw qx qy qz) (Vertex4 pw px py pz) t
  | 1.0 - cosphi < 1e-8 = q
  | otherwise           = ((sin ((1-t)*phi) *^ q) <+> sin (t*phi) *^ f p) ^/ sin phi
  where
    q = Vertex4 qx qy qz qw
    p = Vertex4 px py pz pw
    dqp = dot (toFrogVector q) (toFrogVector p)
    (cosphi, f) = if dqp < 0 then (-dqp, (-1 *^)) else (dqp, id)
    phi = acos cosphi

collectively :: Point3 -> Point4 -> Point3 -> FrogMatrix
collectively pos (Vertex4 x y z s) sc = fromAffine (toFrogList sc) (toFrogList pos) <>
  (4><4) [
    1 - 2*y**2 - 2*z**2, 2*x*y - 2*s*z, 2*x*z + 2*s*y, 0,
    2*x*y + 2*s*z, 1 - 2*x**2 - 2*z**2, 2*y*z - 2*s*x, 0,
    2*x*z - 2*s*y, 2*y*z + 2*s*x, 1 - 2*x**2 - 2*y**2, 0,
    0, 0, 0, 1
  ]

worldify :: Int -> FrogMatrix -> [FrogMatrix] -> [MothBone] -> FrogMatrix
worldify boneIndex world localMatrices bones = do
  let parentIndex = fromIntegral (mother $ bones !! boneIndex)
      localMatrix = localMatrices !! boneIndex
  case parentIndex of
    255 -> localMatrix <> world
    _ -> worldify parentIndex (localMatrix <> world) localMatrices bones

frame :: [a] -> [GLfloat] -> Float -> (a, a, Float)
frame values times now =
  let next = fromMaybe (length times - 1) (findIndex (> now) times)
      current = if next == 0 then 0 else next - 1
      nextT = times !! next
      currentT = times !! current
      nextV = values !! next
      currentV = values !! current
      t = clamp (0, 1) $ (now - currentT) / (nextT - currentT)
  in (currentV, nextV, t)

local3 :: MothFile -> Float -> (MothTale -> Mothly3) -> [Vertex3 GLfloat]
local3 mammoth now prop = map (\mothtale ->
        let Mothly3 positions times = prop mothtale
            (currentV, nextV, t) = frame positions times now
        in currentV <+> (t *^ (nextV <-> currentV))
       ) $ chronicles mammoth

play :: Animation -> Float -> IO [GLfloat]
play animoth now = do
  let mammoth = aMoth animoth
      fossil = skeleton mammoth
      mothNow = mod' (now - aTime animoth) 1.0
      confused = local3 mammoth mothNow MOTH.position
      confounded = map (\mothtale ->
        let Mothly4 quaternions times = MOTH.quaternion mothtale
            (currentQ, nextQ, t) = frame quaternions times mothNow
            Vertex4 y z real x = slerp currentQ nextQ t
        in Vertex4 x y z real
        ) $ chronicles mammoth
      dazed = local3 mammoth mothNow MOTH.scale

  let be = zipWith3
      bonewards = be collectively confused confounded dazed
      ma'ammoth = map (\me -> worldify me (MOTH.matrix (skeleton mammoth !! me)) bonewards fossil) [0..(length fossil - 1)]

  return $ concatMap (toList . flatten) ma'ammoth

makeAssetMesh :: AssetMeshProfile -> IO Mesh
makeAssetMesh mprofile = do
  (Concoction pro hmap path) <- brewProfile (Left mprofile)

  -- read all the data
  bytes <- summon (fromJust path)
  let frogFile = runGet frogify bytes
  print frogFile

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
  print $ boneInfluence frogFile
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

      print $ weightBuffer frogFile

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
