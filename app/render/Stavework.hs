module Stavework (
  stavewrite
, spellgreat
, renderFeather
, Stake (..)
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
import Rime (Point, Polyhedron, (<+>), (^*))
import Shade (Mesh (elementCount, vbo), bufferSize, drawFaces, useMesh, drawMesh)
import State (Stately (staveware))
import Stavemake (Stave (Stave, advance, texture), Stavebook, greatness, sharpness, Staveware)
import Mean (Twain)
import Matrix (FrogVertex((^*^)), RenderView, getPerspectiveMatrix, getOrthographicMatrix)
import Time (Time)
import Numeric.LinearAlgebra (ident)


data Stake = North | South | East | West | Middle deriving (Show, Eq)
type Stakes = Twain Stake

stavewrite :: (Stately b) => Point -> Stakes -> Point -> Blee -> String -> StateT b IO ()
stavewrite stead stakes scale blee spell =
  get >>= \statewit
  -> let (book, mesh) = staveware statewit in lift $
    useMesh mesh

    >> let advances = scanl (+) 0 (map (advance . (book!)) spell)
           offset = reckonStakes book stakes scale spell in

    forM_ (zip [0..] spell) $ \(i, char) ->
      unless (member char book) (error $ "cant write" ++ show char)

      >> let stave = book!char
             vertices = stavenook (offset <+> stead) scale (advances!!i) stave in
         activeTexture $= TextureUnit 0
      >> textureBinding Texture2D $= Just (texture stave)
      >> bindBuffer ArrayBuffer $= Just (vbo mesh)
      >> uniformMap mesh ! "u_blee" >>= ($= bleeToGLVector4 blee) . uniform
      >> withArray vertices (bufferSubData ArrayBuffer WriteToBuffer 0 $ bufferSize vertices)
      >> bindBuffer ArrayBuffer $= Nothing
      >> drawFaces (elementCount mesh)

reckonStakes :: Stavebook -> Stakes -> Point -> String -> Point
reckonStakes book (xstake, ystake) scale spell = Vertex2 ew ns where
  Vertex2 x y = spellgreat book scale spell
  ew = case xstake of
    West -> 0
    Middle -> -(x/4)
    East -> -(x/2)
    _ -> error "bad stake"
  ns = case ystake of
    South -> 0
    Middle -> -(y/4)
    North -> -(y/2)
    _ -> error "bad stake"

spellgreat :: Stavebook -> Point -> String -> Vertex2 GLfloat
spellgreat book scale = (scale ^*^) .
  foldr ((\(Vertex2 x y) (Vertex2 a b) -> Vertex2 (x+a) (max y b))
  . (\(Stave (Vertex2 _ by) (Vertex2 _ _) step _) -> Vertex2 step by)
  . (book!)
  ) (Vertex2 0 0)

stavenook :: Point -> Point -> GLfloat -> Stave -> Polyhedron
stavenook bottomLeft scale step stave = [
    Vertex3 (x+w) (y+h) 0
  , Vertex3 (x+w)  y    0
  , Vertex3  x     y    0
  , Vertex3  x    (y+h) 0
  ] where
    (Stave (Vertex2 left top) z@(Vertex2 _ height) _ _) = stave
    scale' = scale ^* (greatness / fromIntegral sharpness)
    Vertex2 x y = bottomLeft <+> (scale' ^*^ Vertex2 (left + step) (top - height))
    Vertex2 w h = scale' ^*^ z

renderFeather :: RenderView -> Time -> Staveware -> StateT a IO ()
renderFeather dis t ware = lift $ drawMesh
  (getPerspectiveMatrix dis)
  (ident 4)
  (getOrthographicMatrix dis)
  t
  (snd ware)
