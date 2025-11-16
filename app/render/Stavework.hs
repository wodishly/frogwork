{-|
@Stavemake@ is for anything to do with writing the stave.
@Stavework@ imports @State@, so @State@ must not import @Stavework@.
-}
module Stavework (
  Stake (..)
, Writing (..)
, Say (..)
, makeWriting
, renderFeather
, spellgreat
, stavewrite
) where

import Control.Monad (forM_, unless)
import Control.Monad.State (MonadTrans (lift), StateT)
import Data.HashMap.Lazy (member, (!))
import Numeric.LinearAlgebra (ident)

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

import State (Stately)
import Allwit (Allwit (..))

import Blee (Blee, bleeToGLVector4, lightwhelk)
import FastenShade (Programful (uniformMap))
import Matrix (RenderView (size), getOrthographicMatrix, getPerspectiveMatrix)
import Mean (Twain)
import Rime (FrogVertex ((^*^)), Point, Polyhedron, (<+>), (^*))
import Shade (Mesh (elementCount, vbo), bufferSize, drawFaces, drawMesh, useMesh)
import Stavemake (Stave (Stave, advance, texture), Stavebook, greatness, sharpness)
import Time (Timewit (Timewit))


data Stake = North | South | East | West | Middle deriving (Show, Eq)
type Stakes = Twain Stake

data Writing = Writing {
  writ :: String
, stead :: Point
, stakes :: Stakes
, scale :: Point
, blee :: Blee
, wone :: Say
}

data Say = Say {
  start :: Float
, speed :: Float
}

unsay :: Say
unsay = Say 0 0

-- | Twimiddle, truescale, lightwhelk, unwone.
-- Use the orshapework @Writing@ otherwise.
makeWriting :: String -> Point -> Writing
makeWriting w st = Writing w st (Middle, Middle) (Vertex2 1 1) lightwhelk unsay

howmany :: Timewit -> Say -> [Int]
howmany (Timewit now _ ) (Say st sp)
  | now < st = [] -- too early
  | sp == 0 = [0..] -- all at once
  | otherwise = [0..round $ (now-st)/sp]

stavewrite :: Stately a => Allwit -> [Writing] -> StateT a IO ()
stavewrite allwit writings = do
  let (book, mesh) = staveware allwit
  lift $ useMesh mesh

  lift $ forM_ writings $ \(Writing wr std' stk scl' bl say) -> do

    let advances = scanl (+) 0 (map (advance . (book!)) wr)
        (w, h) = size $ display allwit
        scaleyscale = Vertex2 (1/800) (1/600) ^*^ Vertex2 w h
        scl = scaleyscale ^*^ scl'
        std = scaleyscale ^*^ std'
        -- offset = Vertex2 ((/(10 :: Float)) . fromIntegral $ lifetime time) 0 <+> reckonStakes book stk scl wr
        offset = reckonStakes book stk scl wr

    forM_ (zip (howmany (timewit allwit) say) wr) $ \(i, char) -> do
      unless (member char book) (error $ "cant write" ++ show char)

      let stave = book!char
          vertices = stavenook (offset <+> std) scl (advances!!i) stave

      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just (texture stave)
      bindBuffer ArrayBuffer $= Just (vbo mesh)
      uniformMap mesh ! "u_blee" >>= ($= bleeToGLVector4 bl) . uniform
      withArray vertices (bufferSubData ArrayBuffer WriteToBuffer 0 $ bufferSize vertices)
      bindBuffer ArrayBuffer $= Nothing
      drawFaces (elementCount mesh)

reckonStakes :: Stavebook -> Stakes -> Point -> String -> Point
reckonStakes book (xstake, ystake) scl wr = Vertex2 ew ns where
  Vertex2 x y = spellgreat book scl wr
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
spellgreat book scl = (scl ^*^) . foldr
  ((\(Vertex2 x y) (Vertex2 a b) -> Vertex2 (x+a) (max y b))
  . (\(Stave (Vertex2 _ by) (Vertex2 _ _) step _) -> Vertex2 step by)
  . (book!))
  (Vertex2 0 0)

stavenook :: Point -> Point -> GLfloat -> Stave -> Polyhedron
stavenook bottomLeft scl step stave = [
    Vertex3 (x+w) (y+h) 0
  , Vertex3 (x+w)  y    0
  , Vertex3  x     y    0
  , Vertex3  x    (y+h) 0
  ] where
    (Stave (Vertex2 left top) z@(Vertex2 _ height) _ _) = stave
    scale' = scl ^* (greatness / fromIntegral sharpness)
    Vertex2 x y = bottomLeft <+> (scale' ^*^ Vertex2 (left + step) (top - height))
    Vertex2 w h = scale' ^*^ z

renderFeather :: Stately a => Allwit -> StateT a IO ()
renderFeather allwit = lift $ drawMesh
  (getPerspectiveMatrix $ display allwit)
  (ident 4)
  (getOrthographicMatrix $ display allwit)
  (timewit allwit)
  (snd $ staveware allwit)
