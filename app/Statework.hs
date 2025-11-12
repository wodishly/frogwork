{-# LANGUAGE FlexibleInstances #-}
module Statework (
  stavewrite
, spellgreat
) where

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
import State (Stately (staveware), preent)
import Stavemake (Stave (Stave, advance, texture), Stavebook, greatness, sharpness)

class Spellstead a where
  nook :: a -> GLfloat -> GLfloat -> Stave -> Polyhedron

data Stead = Middle deriving (Show, Eq)
instance Spellstead Stead where

stavewrite :: (Spellstead a, Stately b) => a -> GLfloat -> Blee -> String -> StateT b IO ()
stavewrite stead scale blee spell = do
  statewit <- get
  let (book, mesh) = staveware statewit
  lift $ do
    useMesh mesh

    let advances = scanl (+) 0 (map (advance . (book!)) spell)

    forM_ (zip [0..] spell) $ \(i, char) -> do
      unless (member char book) (error $ "cant write" ++ show char)

      let stave = book!char
          vertices = nook stead scale (advances!!i) stave

      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just (texture stave)
      bindBuffer ArrayBuffer $= Just (vbo mesh)

      uniformMap mesh ! "u_blee" >>= ($= bleeToGLVector4 blee) . uniform

      withArray vertices (bufferSubData ArrayBuffer WriteToBuffer 0 $ bufferSize vertices)

      bindBuffer ArrayBuffer $= Nothing
      drawFaces $ elementCount mesh

spellgreat :: Stavebook -> String -> Vertex2 GLfloat
spellgreat book = foldr (
  (\(Vertex2 x y) (Vertex2 a b) -> Vertex2 (x+a) (max y b))
  . (\(Stave _ (Vertex2 w h) step _) -> Vertex2 (w+step) h)
  . (book!)) (Vertex2 0 0)

instance Spellstead (Vertex2 GLfloat) where
  nook bottomLeft scale step stave = [
      Vertex3 (x+w) (y+h) 0
    , Vertex3 (x+w)  y    0
    , Vertex3  x     y    0
    , Vertex3  x    (y+h) 0
    ] where
      (Stave (Vertex2 left top) z@(Vertex2 _ height) _ _) = stave
      scale' = scale * greatness / fromIntegral sharpness
      Vertex2 x y = bottomLeft <+> (scale' *^ Vertex2 (left + step) (top - height))
      Vertex2 w h = scale' *^ z
