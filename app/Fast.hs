module Fast where

framegoal :: Num a => a
framegoal = 60

framefulness :: Num a => a
framefulness = 4

shaderBasePath :: String
shaderBasePath = "app/render/shaders"

assetsBasePath :: String
assetsBasePath = "assets"
