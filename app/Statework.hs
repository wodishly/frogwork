module Statework where

import Control.Monad (forM_, unless)
import Control.Monad.State (MonadState (get), MonadTrans (lift), StateT)
import Data.HashMap.Lazy (member, (!))

import Foreign (withArray)
import Graphics.Rendering.OpenGL (
    BufferTarget (ArrayBuffer)
  , GLfloat
  , HasSetter (($=))
  , TextureTarget2D (Texture2D)
  , TextureUnit (TextureUnit)
  , TransferDirection (WriteToBuffer)
  , Uniform (uniform)
  , Vertex2 (Vertex2)
  , Vertex3 (Vertex3)
  , activeTexture
  , bindBuffer
  , bufferSubData
  , textureBinding
  )

import Blee (Blee, bleeToGLVector4)
import FastenShade (Programful (uniformMap))
import Rime (Point, Polyhedron, (*^), (<+>))
import Shade (Mesh (elementCount, vbo), bufferSize, drawFaces, useMesh)
import State (Stately (staveware))
import Stavemake (Stave (Stave, advance, texture), greatness, sharpness, Stavebook)


stavewrite :: Stately a => Point -> GLfloat -> Blee -> String -> StateT a IO ()
stavewrite bottomLeft scale blee spell = do
  statewit <- get
  let (book, mesh) = staveware statewit
  lift $ do
    useMesh mesh

    let advances = scanl (+) 0 (map (advance . (book!)) spell)

    forM_ (zip [0..] spell) $ \(i, char) -> do
      unless (member char book) (error $ "cant write" ++ show char)

      let stave = book!char
          vertices = stavenooks stave scale bottomLeft (advances!!i)

      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just (texture stave)
      bindBuffer ArrayBuffer $= Just (vbo mesh)

      uniformMap mesh ! "u_blee" >>= ($= bleeToGLVector4 blee) . uniform

      withArray vertices (bufferSubData ArrayBuffer WriteToBuffer 0 $ bufferSize vertices)

      bindBuffer ArrayBuffer $= Nothing
      drawFaces $ elementCount mesh

spellwidth :: Stavebook -> String -> Vertex2 GLfloat
spellwidth book spell = 

stavenooks :: Stave -> GLfloat -> Point -> GLfloat -> Polyhedron
stavenooks stave scale bottomLeft step = [
    Vertex3 (x+w) (y+h) 0
  , Vertex3 (x+w)  y    0
  , Vertex3  x     y    0
  , Vertex3  x    (y+h) 0
  ] where
    (Stave (Vertex2 left top) z@(Vertex2 _ height) _ _) = stave
    scale' = scale * greatness / fromIntegral sharpness
    Vertex2 x y = bottomLeft <+> (scale' *^ Vertex2 (left + step) (top - height))
    Vertex2 w h = scale' *^ z
