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
  , Vertex3 (Vertex3)
  )

import Matrix (Polyhedron, Polygon)
import Mean (Twain)


type MeshProfile = Either AssetMeshProfile SimpleMeshProfile
type UniformMap = HashMap [Char] (GettableStateVar UniformLocation)

class Shaderful a where
  shaderProfile :: a -> ShaderProfile

class Monad m => Pathlikeful m a where
  filePath :: a -> m String

class Programful a where
  program :: a -> Program
  uniformMap :: a -> UniformMap

-- data sent to the renderer when requesting a mesh
newtype AssetMeshProfile = AssetMeshProfile String deriving (Show, Eq)

instance Shaderful AssetMeshProfile where
  shaderProfile _ = defaultAssetShaderProfile

instance Pathlikeful Identity AssetMeshProfile where
  filePath (AssetMeshProfile s) = Identity s

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
  , uniforms = ["u_projection_matrix", "u_model_matrix", "u_texture", "u_view_matrix", "u_time"]
}

defaultSimpleMeshProfile :: SimpleMeshProfile
defaultSimpleMeshProfile = SimpleMeshProfile {
    vbuffer = floorVBuffer
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

floorVBuffer :: Polyhedron
floorVBuffer = [
    Vertex3  20  0  20.0 --NE
  , Vertex3  20 -0 -20.0 --SE
  , Vertex3 -20 -0 -20.0 --SW
  , Vertex3 -20  0  20.0 --NW
  ]

staveVBuffer :: Polyhedron
staveVBuffer = [
    Vertex3   1 -(1/4) 0 --NE
  , Vertex3   1 -(3/4) 0 --SE
  , Vertex3  -1 -(3/4) 0 --SW
  , Vertex3  -1 -(1/4) 0 --NW
  ]

iBuffer :: [Word32]
iBuffer = [
    0, 1, 2
  , 2, 3, 0
  ]

quadUvBuffer :: Polygon
quadUvBuffer = [
    Vertex2 1 0
  , Vertex2 1 1
  , Vertex2 0 1
  , Vertex2 0 0
  ]
