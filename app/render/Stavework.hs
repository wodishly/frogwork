{-|
@Stavemake@ is for anything to do with writing the stave.
@Stavework@ imports @State@, so @State@ must not import @Stavework@.
-}
module Stavework (
  Stake (..)
, Writing (..)
, Speechframe (..)
, makeSpeechframe
, speechwrite
, makeWriting
, renderFeather
, spellframe
, stavewrite
) where

import Control.Monad (forM_, unless)
import Control.Monad.State (MonadTrans (lift), StateT)
import Data.HashMap.Lazy (member, (!))
import Data.List (find, unfoldr)
import Data.Maybe (fromMaybe)

import Foreign (withArray)
import Numeric.LinearAlgebra (ident)
import Graphics.Rendering.OpenGL (
    BufferTarget (ArrayBuffer)
  , GLfloat
  , HasSetter (($=))
  , TextureTarget2D (Texture2D)
  , TextureUnit (TextureUnit)
  , TransferDirection (WriteToBuffer)
  , Uniform (uniform)
  , Vertex2 (Vertex2)
  , activeTexture
  , bindBuffer
  , bufferSubData
  , textureBinding
  )

import State (Stately)
import Allwit (Allwit (..))

import Blee (Blee, bleeToGLVector4, lightwhelk, white)
import FastenMain (orheight, orwidth)
import FastenShade (Programful (uniformMap))
import Matrix (RenderView (size), getOrthographicMatrix, getPerspectiveMatrix)
import Mean (Twain, allIn, ly')
import Rime (Axle (Z), FrogVertex (nonehood, onehood, (^*^)), Point, Polyhedron, fournook, inject, (*^), (<+>), (<->), (^*), Fournook (..))
import Shade (Mesh (elementCount, vbo), bufferSize, drawFaces, drawMesh, useMesh)
import Stavemake (Stave (Stave, advance, texture), Stavebook)


data Stake = North | South | East | West | Middle deriving (Show, Eq)
type Stakes = Twain Stake

data Writing = Writing {
  stakes :: Stakes
, scale :: Point
, blee :: Blee
, stead :: Point
, writ :: String
}

-- | Twimiddle, truescale, lightwhelk, unwone.
-- Use the orshapework @Writing@ otherwise.
makeWriting :: Point -> String -> Writing
makeWriting = Writing (Middle, Middle) onehood lightwhelk

data Speechframe = Speechframe {
  nooks :: Fournook
, rim :: GLfloat
, skale :: Point
, meesh :: Mesh
, speech :: String
}

makeSpeechframe :: Mesh -> String -> Speechframe
makeSpeechframe = Speechframe
  (Fournook (Vertex2 50 (orheight/2-75)) (Vertex2 (orwidth-100) (orheight-50)))
  22.5
  ((1/3) *^ onehood)

speechwrite :: Stately a => Allwit -> Speechframe -> StateT a IO ()
speechwrite allwit speechframe = stavewrite allwit
  . flayLines
  . wrapToFrame allwit speechframe (fst $ staveware allwit)
  $ speech speechframe

wrapToFrame :: Allwit -> Speechframe -> Stavebook -> String -> [Writing]
wrapToFrame allwit frame book string
  | isOverflow allwit frame book string
    = (\(l, r) -> writeSpeechOnFrame frame l : wrapToFrame allwit frame book (tail r))
      (sunder allwit frame book string)
  | otherwise = [writeSpeechOnFrame frame string]

sunder :: Allwit -> Speechframe -> Stavebook -> String -> Twain String
sunder allwit frame book string = splitAt (fromMaybe (length string)
    (find (allIn [(== ' ') . (string!!), isLastBeforeOverflow allwit frame book string]) [0..])
  ) string

isLastBeforeOverflow :: Allwit -> Speechframe -> Stavebook -> String -> Int -> Bool
isLastBeforeOverflow allwit frame book string i = any
  (isOverflow allwit frame book . (++ take i string))
  (unfoldr
    (\s -> if null s then Nothing else Just (init s, init s))
    (takeWhile (/= ' ') (drop (i+1) string)))

-- | Tells if the string is too long for the speechframe.
isOverflow :: Allwit -> Speechframe -> Stavebook -> String -> Bool
isOverflow allwit frame book string = w_spell > w_frame
  where Vertex2 w_frame _ = ly' ((++"two") . show) $ scaleToScreen allwit $ ly' ((++"one") . show) $ greatness (nooks frame) <-> (2 *^ Vertex2 (rim frame) 0)
        Vertex2 w_spell _ = ly' ((++"three") . show) $ spellframe book (skale frame) string

-- | Flays each line of writing below the last.
--
-- todo: draw out the @32@
flayLines :: [Writing] -> [Writing]
flayLines = zipWith (\i wr -> wr { stead = Vertex2 0 (-32*i) <+> stead wr }) [0..]

-- | Fits the speech to the frame.
writeSpeechOnFrame :: Speechframe -> String -> Writing
writeSpeechOnFrame frame = Writing
  (West, North)
  (skale frame)
  white
  (topLeft (nooks frame) <+> Vertex2 (rim frame) -(rim frame))

scaleToScreen :: Allwit -> Point -> Point
scaleToScreen allwit = (^*^) (Vertex2 (w/orwidth) (h/orheight))
  where (w, h) = size (display allwit)

-- | The great work.
stavewrite :: Stately a => Allwit -> [Writing] -> StateT a IO ()
stavewrite allwit writings = do

  let (book, mish) = staveware allwit

  lift $ useMesh mish
  lift $ forM_ writings $ \(Writing stk scl' bl std' wr) -> do

    let advances = scanl (+) 0 (map (advance . (book!)) wr)
        scl = scaleToScreen allwit scl'
        std = scaleToScreen allwit std'
        offset = reckonStakes book stk scl wr

    forM_ (zip [0..] wr) $ \(i, char) -> do
    -- forM_ (zip (howmany (timewit allwit) ww) wr) $ \(i, char) -> do
      unless (member char book) (error $ "cant write " ++ show char)

      let stave = book!char
          vertices = stavenook (offset <+> std) scl (advances!!i) stave

      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just (texture stave)
      bindBuffer ArrayBuffer $= Just (vbo mish)
      uniformMap mish ! "u_blee" >>= ($= bleeToGLVector4 bl) . uniform
      withArray vertices (bufferSubData ArrayBuffer WriteToBuffer 0 $ bufferSize vertices)
      bindBuffer ArrayBuffer $= Nothing
      drawFaces (elementCount mish)

reckonStakes :: Stavebook -> Stakes -> Point -> String -> Point
reckonStakes book (xstake, ystake) scl wr = Vertex2 ew ns where
  Vertex2 x y = spellframe book scl wr
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

-- | Reckon the spell's bounding frame.
spellframe :: Stavebook -> Point -> String -> Point
spellframe book scl = (scl ^*^) . foldr (frameyoke . staveframe . (book!)) nonehood

-- | Yoke two staves' bounding frames.
frameyoke :: Point -> Point -> Point
frameyoke (Vertex2 x y) (Vertex2 a b) = Vertex2 (x+a) (max y b)

-- | Reckon the stave's bounding frame.
staveframe :: Stave -> Point
staveframe (Stave (Vertex2 _ by) (Vertex2 _ _) step _) = Vertex2 step by

-- | Reckon the stave's fournook.
stavenook :: Point -> Point -> GLfloat -> Stave -> Polyhedron
stavenook bottomLeft scl step stave = inject Z <$> fournook (Vertex2 (x+w) (y+h)) (Vertex2 x y) where
    (Stave (Vertex2 left top) z@(Vertex2 _ height) _ _) = stave
    scale' = scl ^* (1/2)
    Vertex2 x y = bottomLeft <+> (scale' ^*^ Vertex2 (left+step) (top-height))
    Vertex2 w h = scale' ^*^ z

renderFeather :: Stately a => Allwit -> StateT a IO ()
renderFeather allwit = lift $ drawMesh
  (getPerspectiveMatrix $ display allwit)
  (ident 4)
  (getOrthographicMatrix $ display allwit)
  (timewit allwit)
  (snd $ staveware allwit)
