{-|
@Stavemake@ is for anything to do with writing the stave.
@Stavework@ imports @State@, so @State@ must not import @Stavework@.
-}
module Stavework (
  Speechframe (..)
, Writing (..)
  , blee
  , throoks
, makeSpeechframe
, speechwrite
, makeWriting
, renderFeather
, stringframe
, stavewriteAll
) where

import Control.Lens (makeLenses, (^.), (?~))
import Control.Monad (forM_, forM, void)
import Control.Monad.State (MonadTrans (lift), StateT)
import Data.HashMap.Lazy (member, (!))
import Data.List (find, unfoldr)
import Data.Maybe (fromJust, fromMaybe)

import Foreign (withArray)
import Numeric.LinearAlgebra (ident)
import Graphics.Rendering.OpenGL (
    BufferTarget (ArrayBuffer)
  , GLfloat
  , TextureTarget2D (Texture2D)
  , TextureUnit (TextureUnit)
  , TransferDirection (WriteToBuffer)
  , Uniform (uniform)
  , Vertex2 (Vertex2)
  , activeTexture
  , bindBuffer
  , bufferSubData
  , textureBinding
  , ($=)
  )

import Allwit (Allwit (..))
import FastenMain (Stake (..), Stakes, orheight, orwidth, orwindow, orwest)
import FastenShade (Programful (uniformMap))
import State (Stately)

import Blee (Blee, bleeToGLVector4, lightwhelk, white)
import Matrix (RenderView (size), getOrthographicMatrix, getPerspectiveMatrix)
import Mean (Twain, allIn, ly')
import Rime
import Shade (Mesh (elementCount, vbo), bufferSize, drawFaces, drawMesh, useMesh)
import Stavemake (Stave (Stave, advance, texture), Stavebook)


data Writing = Writing {
  _stakes :: Stakes
, _scale :: Point
, _blee :: Blee
, _throoks :: Maybe [Polyhedron]
, _stead :: Point
, _writ :: String
}
makeLenses ''Writing

-- | Twimiddle, truescale, lightwhelk, unwone.
-- Use the orshapework @Writing@ otherwise.
makeWriting :: Point -> String -> Writing
makeWriting = Writing (Middle, Middle) onehood lightwhelk Nothing

data Speechframe = Speechframe {
  rim :: GLfloat
, skale :: Point
, nooks :: Fournook
, meesh :: Mesh
, speech :: String
}

makeSpeechframe :: Mesh -> String -> Speechframe
makeSpeechframe = Speechframe 22.5 ((1/3) *^ onehood) $ Fournook
  (orwest <+> Vertex2 50 -75)
  (orwindow <-> Vertex2 -100 -50)

speechwrite :: Stately a => Allwit -> Speechframe -> StateT a IO ()
speechwrite allwit speechframe = void $ stavewriteAll allwit
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
        Vertex2 w_spell _ = ly' ((++"three") . show) $ stringframe book (skale frame) string

-- | Flays each line of writing below the last.
--
-- todo: draw out the @32@
flayLines :: [Writing] -> [Writing]
flayLines = zipWith (\i wr -> wr { _stead = Vertex2 0 (-32*i) <+> wr^.stead }) [0..]

-- | Fits the speech to the frame.
writeSpeechOnFrame :: Speechframe -> String -> Writing
writeSpeechOnFrame frame = Writing
  (West, North)
  (skale frame)
  white
  Nothing
  (topLeft (nooks frame) <+> Vertex2 (rim frame) -(rim frame))

scaleToScreen :: Allwit -> Point -> Point
scaleToScreen allwit = (^*^) (Vertex2 (w/orwidth) (h/orheight))
  where (w, h) = size (display allwit)

allreckon :: Allwit -> Writing -> [Polyhedron]
allreckon allwit (Writing stk scl' _ _ std' wr) =
  let book = fst $ staveware allwit
      advances = scanl (+) 0 (map (advance . (book!)) wr)
      scl = scaleToScreen allwit scl'
      std = scaleToScreen allwit std'
      offset = reckonStakes book stk scl wr
  in ly' (const "lets do it again") zipWith
      (\ i char -> stavenook (offset <+> std) scl (advances !! i) (book ! char))
      [0..] wr

-- | The greater work.
stavewriteAll :: Stately a => Allwit -> [Writing] -> StateT a IO [Writing]
stavewriteAll allwit writings = do
  let (_, mish) = staveware allwit

  lift $ useMesh mish
  forM writings (stavewrite allwit)

-- | The great work.
stavewrite :: Stately a => Allwit -> Writing -> StateT a IO Writing
stavewrite allwit writing = do
  let writing' = (throoks ?~ fromMaybe (allreckon allwit writing) (writing^.throoks)) writing
      (book, mish) = staveware allwit

  forM_ (zip [0..] (writing^.writ)) $ \(i, char) ->
    if member char book
    then lift $ carve (book!char) (fromJust (writing'^.throoks)!!i) mish (writing'^.blee)
    else error $ show char ++ " is not in the book"

  return writing'

-- | Carves the staves into being.
carve :: Stave -> Polyhedron -> Mesh -> Blee -> IO ()
carve stave vertices mish bl = do
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just (texture stave)
  bindBuffer ArrayBuffer $= Just (vbo mish)
  uniformMap mish ! "u_blee" >>= ($= bleeToGLVector4 bl) . uniform
  withArray vertices (bufferSubData ArrayBuffer WriteToBuffer 0 $ bufferSize vertices)
  bindBuffer ArrayBuffer $= Nothing
  drawFaces (elementCount mish)

renderFeather :: Stately a => Allwit -> StateT a IO ()
renderFeather allwit = lift $ drawMesh
  (getPerspectiveMatrix $ display allwit)
  (ident 4)
  (getOrthographicMatrix $ display allwit)
  (timewit allwit)
  (snd $ staveware allwit)

reckonStakes :: Stavebook -> Stakes -> Point -> String -> Point
reckonStakes book (xstake, ystake) scl wr = Vertex2 ew ns where
  Vertex2 x y = stringframe book scl wr
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

-- | Reckon the bounding frame of a whole string.
stringframe :: Stavebook -> Point -> String -> Point
stringframe book scl = (scl ^*^) . foldr (yokeFrames . staveframe . (book!)) nonehood

-- | Yoke a twain of bounding frames.
yokeFrames :: Point -> Point -> Point
yokeFrames (Vertex2 x y) (Vertex2 a b) = Vertex2 (x+a) (max y b)

-- | Reckon the bounding frame of a stave.
staveframe :: Stave -> Point
staveframe (Stave (Vertex2 _ by) (Vertex2 _ _) step _) = Vertex2 step by

-- | Reckon the nooks of a stave.
stavenook :: Point -> Point -> GLfloat -> Stave -> Polyhedron
stavenook bottomLeft scl step stave = inject Z <$> fournook (z0 <+> z1) z0
  where
    (Stave (Vertex2 left top) great@(Vertex2 _ height) _ _) = stave
    scl' = scl ^* (1/2)
    z0 = bottomLeft <+> (scl' ^*^ Vertex2 (left+step) (top-height))
    z1 = scl' ^*^ great
