module Matrix where
import Graphics.Rendering.OpenGL (GLfloat)
import Data.Vector.Storable (Vector, fromList)
  
type Transform = Vector GLfloat

identity :: Transform
identity = fromList [
  1,0,0,0,
  0,1,0,0,
  0,0,1,0,
  0,0,0,1
  ]

fromTranslation :: GLfloat -> GLfloat -> GLfloat -> Transform
fromTranslation x y z = fromList [
  1,0,0,x,
  0,1,0,y,
  0,0,1,z,
  0,0,0,1
  ]
