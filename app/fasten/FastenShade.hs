{- HLINT ignore "Use ++" -}
module FastenShade where

import Data.HashMap.Lazy (HashMap)

import Foreign (Word32)
import Graphics.Rendering.OpenGL
  ( GettableStateVar,
    TextureObject,
    UniformLocation,
  )

import Mean
import Rime
import FastenFrame
import Strike


type MeshProfile = Either AssetMeshProfile SimpleMeshProfile
type UniformMap = HashMap [Char] (GettableStateVar UniformLocation)

class Shaderful a where
  shaderProfile :: a -> ShaderProfile

class Monad m => Pathlikeful m a where
  frogFilePath :: a -> m String

-- data sent to the renderer when requesting a mesh
newtype AssetMeshProfile = AssetMeshProfile {
  road :: String
} deriving (Show, Eq)

instance Shaderful AssetMeshProfile where
  shaderProfile _ = defaultAssetShaderProfile

data SimpleMeshProfile = SimpleMeshProfile {
    vbuffer :: Polyhedron
  , ibuffer :: [Word32]
  , meshShaderProfile :: ShaderProfile
  , uvbuffer :: Maybe [Vertex2 GLfloat]
  , texObject :: Maybe TextureObject
} deriving (Show, Eq)

instance Shaderful SimpleMeshProfile where
  shaderProfile = meshShaderProfile

data ShaderProfile = ShaderProfile {
  -- name of (vertex, fragment) shader GLSL files (without extension)
    names :: Twain FilePath
  -- uniform symbol names
  , uniforms :: [String]
} deriving (Show, Eq)

defaultAssetMeshProfile :: AssetMeshProfile
defaultAssetMeshProfile = AssetMeshProfile "test"

defaultAssetShaderProfile :: ShaderProfile
defaultAssetShaderProfile = ShaderProfile {
    names = ("vertex", "texture_fragment")
  , uniforms = ["u_projection_matrix", "u_model_matrix", "u_texture", "u_view_matrix", "u_bone_matrices", "u_time"]
}

defaultSimpleMeshProfile :: SimpleMeshProfile
defaultSimpleMeshProfile = SimpleMeshProfile {
    vbuffer = inject Y <$> fournook (Vertex2 20 -20) (Vertex2 -20 20)
  , ibuffer = fournookBuffer
  , uvbuffer = Nothing
  , meshShaderProfile = defaultSimpleShaderProfile
  , texObject = Nothing
}

defaultSimpleShaderProfile :: ShaderProfile
defaultSimpleShaderProfile = ShaderProfile {
    names = ("vertex_sheet", "color_fragment")
  , uniforms = ["u_projection_matrix", "u_model_matrix", "u_view_matrix", "u_time"]
}

staveMeshProfile :: SimpleMeshProfile
staveMeshProfile = SimpleMeshProfile {
  vbuffer = inject Z <$> fournook (Vertex2 1 -1) (Vertex2 -1 1)
, ibuffer = fournookBuffer
, uvbuffer = Just $ fournook (Vertex2 1 0) (Vertex2 0 1)
, meshShaderProfile = ShaderProfile
    (shadersOf "stave")
    ["u_texture", "u_time", "u_orthographic_matrix", "u_blee"]
, texObject = Nothing
}

speechMeshProfile :: SimpleMeshProfile
speechMeshProfile = SimpleMeshProfile {
    vbuffer = inject Z <$> fournook (Vertex2 1 -1) (Vertex2 -1 1)
  --  vbuffer = inject Z <$> fournook (Vertex2 (7/8) (-7/8)) (Vertex2 (-7/8) (-1/4))
  , ibuffer = fournookBuffer
  , uvbuffer = Nothing
  , meshShaderProfile = ShaderProfile (shadersOf "speech") []
  , texObject = Nothing
}

swizzleL :: [a] -> [a]
swizzleL xs = tail xs ++ [head xs]

swizzleR :: [a] -> [a]
swizzleR xs = last xs : init xs

frameMeshProfile :: SimpleMeshProfile
frameMeshProfile = frameMeshProfileOf "frame"

frameMeshProfileOf :: String -> SimpleMeshProfile
frameMeshProfileOf s = SimpleMeshProfile {
    vbuffer = frame' onespit,
    ibuffer = eightnookBuffer,
    uvbuffer = Nothing,
    meshShaderProfile = ShaderProfile
      ("vertex_frame", "fragment_" ++ s)
      ("u_stricken" : uniforms defaultSimpleShaderProfile),
    texObject = Nothing
}

shadersOf :: String -> Twain FilePath
shadersOf s = ("vertex_" ++ s, "fragment_" ++ s)

fournookBuffer :: [Word32]
fournookBuffer = [
    0, 1, 2,
    2, 3, 0
  ]

eightnookBuffer :: [Word32]
eightnookBuffer = [
  -- front
  0, 1, 2,
  2, 3, 0,
  -- right
  1, 5, 6,
  6, 2, 1,
  -- back
  7, 6, 5,
  5, 4, 7,
  -- left
  4, 0, 3,
  3, 7, 4,
  -- bottom
  4, 5, 1,
  1, 0, 4,
  -- top
  3, 2, 6,
  6, 7, 3
  ]

pattern BUNNY_WALK
      , BUNNY_JUMP
      , BUNNY_RUN
      , BUNNY_IDLE
     :: (Eq a, Num a) => a
pattern BUNNY_WALK = 0
pattern BUNNY_JUMP = 2
pattern BUNNY_RUN = 4
pattern BUNNY_IDLE = 5
