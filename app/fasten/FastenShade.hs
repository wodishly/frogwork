module FastenShade where
import Foreign (Word32)
import Matrix (Polyhedron)
import Graphics.Rendering.OpenGL (Vertex3(Vertex3))

data ShaderKind = Vertex | Fragment deriving (Show, Eq)
type MeshProfile = Either AssetMeshProfile SimpleMeshProfile

class Shaderful a where
  shaderProfile :: a -> ShaderProfile

-- data sent to the renderer when requesting a mesh
data AssetMeshProfile = AssetMeshProfile {
    aMeshFileName :: String
}
instance Shaderful AssetMeshProfile where
  shaderProfile _ = defaultAssetShaderProfile

data SimpleMeshProfile = SimpleMeshProfile {
    vbuffer :: Polyhedron
  , ibuffer :: [Word32]
}
instance Shaderful SimpleMeshProfile where
  shaderProfile _ = defaultSimpleShaderProfile

data ShaderProfile = ShaderProfile {
  -- name of (vertex, fragment) shader GLSL files (without extension)
    names :: (String, String)
  -- uniform symbol names
  , uniforms :: [String]
}

defaultAssetMeshProfile :: AssetMeshProfile
defaultAssetMeshProfile = AssetMeshProfile {
  aMeshFileName = "test"
}

defaultAssetShaderProfile :: ShaderProfile
defaultAssetShaderProfile = ShaderProfile {
    names = ("vertex", "texture_fragment")
  , uniforms = ["u_projection_matrix", "u_model_matrix", "u_texture", "u_view_matrix", "u_time"]
}

defaultSimpleMeshProfile :: SimpleMeshProfile
defaultSimpleMeshProfile = SimpleMeshProfile {
    vbuffer = floorVbuffer
  , ibuffer = floorIbuffer
}

defaultSimpleShaderProfile :: ShaderProfile
defaultSimpleShaderProfile = ShaderProfile {
    names = ("vertex_sheet", "color_fragment")
  , uniforms = ["u_projection_matrix", "u_model_matrix", "u_view_matrix", "u_time"]
}

floorVbuffer :: Polyhedron
floorVbuffer = [
    Vertex3 -1 -0   1.0 --SW
  , Vertex3 -1 -1.0 1.0 --NW
  , Vertex3  1 -1.0 1.0 --NE
  , Vertex3  1 -0   1.0 --SE
  ]

floorIbuffer :: [Word32]
floorIbuffer = [
    0, 1, 2
  , 2, 3, 0
  ]
