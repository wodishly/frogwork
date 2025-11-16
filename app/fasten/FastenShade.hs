module FastenShade where

import Control.Monad.Identity (Identity (Identity))
import Data.HashMap.Lazy (HashMap)

import Foreign (Word32)
import Graphics.Rendering.OpenGL (
    GLfloat
  , GettableStateVar
  , Program
  , TextureObject
  , UniformLocation
  , Vertex2 (Vertex2)
  )

import Mean (Twain)
import Rime (Axle (..), Polyhedron, fournook, inject)


type MeshProfile = Either AssetMeshProfile SimpleMeshProfile
type UniformMap = HashMap [Char] (GettableStateVar UniformLocation)

class Shaderful a where
  shaderProfile :: a -> ShaderProfile

class Monad m => Pathlikeful m a where
  frogFilePath :: a -> m String

class Programful a where
  program :: a -> Program
  uniformMap :: a -> UniformMap

-- data sent to the renderer when requesting a mesh
newtype AssetMeshProfile = AssetMeshProfile String deriving (Show, Eq)

instance Shaderful AssetMeshProfile where
  shaderProfile _ = defaultAssetShaderProfile

instance Pathlikeful Identity AssetMeshProfile where
  frogFilePath (AssetMeshProfile s) = Identity s

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
  , ibuffer = iBuffer
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
, ibuffer = iBuffer
, uvbuffer = Just $ fournook (Vertex2 1 0) (Vertex2 0 1)
, meshShaderProfile = ShaderProfile
    (shadersOf "stave")
    ["u_texture", "u_time", "u_orthographic_matrix", "u_blee"]
, texObject = Nothing
}

speechMeshProfile :: SimpleMeshProfile
speechMeshProfile = SimpleMeshProfile {
    vbuffer = inject Z <$> fournook (Vertex2 (7/8) (-7/8)) (Vertex2 (-7/8) (-1/4))
  , ibuffer = iBuffer
  , uvbuffer = Nothing
  , meshShaderProfile = ShaderProfile (shadersOf "speech") []
  , texObject = Nothing
}

shadersOf :: String -> Twain FilePath
shadersOf s = ("vertex_" ++ s, "fragment_" ++ s)

iBuffer :: [Word32]
iBuffer = [
    0, 1, 2
  , 2, 3, 0
  ]

pattern BUNNY_WALK
      , BUNNY_JUMP
      , BUNNY_IDLE
     :: (Eq a, Num a) => a
pattern BUNNY_WALK = 0 
pattern BUNNY_JUMP = 2
pattern BUNNY_IDLE = 5 
