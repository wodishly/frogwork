{-|
@Stavemake@ is for anything to do with writing the stave.
@Stavework@ imports @State@, so @State@ must not import @Stavework@.
-}
module Stavework (
  Speechframe (..),
    writings,
  Writing (..),
    blee,
    throoks,
  makeSpeechframe,
  speechwrite,
  makeWriting,
  renderFeather,
  stringframe,
  stavewriteAll,
) where

import Control.Lens (makeLenses, (?~), (^.))
import Control.Monad (forM_)
import Control.Monad.State (MonadTrans (lift), StateT)
import Data.Bifunctor (Bifunctor (bimap, second))
import Data.HashMap.Lazy (member, (!))
import Data.Maybe (fromJust, fromMaybe)

import Foreign (withArray)
import Numeric.LinearAlgebra (ident)
import Graphics.Rendering.OpenGL (
  BufferTarget (ArrayBuffer),
  GLfloat,
  TextureTarget2D (Texture2D),
  TextureUnit (TextureUnit),
  TransferDirection (WriteToBuffer),
  Uniform (uniform),
  Vertex2 (Vertex2),
  activeTexture,
  bindBuffer,
  bufferSubData,
  textureBinding,
  ($=)
  )

import Allwit (Allwit (..))
import State (Stately)

import Blee (Blee, bleeToGLVector4, lightwhelk, white)
import FastenMain (Stake (..), Stakes, orheight, orwest, orwidth, orwindow)
import FastenShade (Programful (uniformMap))
import Matrix (RenderView (size, RenderView), getOrthographicMatrix, getPerspectiveMatrix)
import Mean (twin, twimap)
import Rime
import Shade (Mesh (elementCount, vbo), bufferSize, drawFaces, drawMesh, useMesh)
import Stavemake (Stave (Stave, advance, texture), Stavebook)


type Scale = Point

data Writing = Writing {
  stakes :: Stakes,
  scale :: Scale,
  _blee :: Blee,
  _throoks :: Maybe [Polyhedron],
  _stead :: Point,
  _writ :: String
}
makeLenses ''Writing

-- | Twimiddle, truescale, lightwhelk, unwone.
-- Otherwise brook the orshapework @Writing@.
makeWriting :: Point -> String -> Writing
makeWriting = Writing (Middle, Middle) onehood lightwhelk Nothing

data Speechframe = Speechframe {
  rim :: GLfloat,
  skale :: Scale,
  _writings :: Maybe [Writing],
  nooks :: Fournook,
  meesh :: Mesh,
  speech :: String
}
makeLenses ''Speechframe

makeSpeechframe :: Mesh -> String -> Speechframe
makeSpeechframe = Speechframe 22.5 ((1/3) *^ onehood) Nothing
  (Fournook (orwest <+> Vertex2 50 -75) (orwindow <-> Vertex2 100 50))

speechwrite :: Stately a => Allwit -> Speechframe -> StateT a IO Speechframe
speechwrite wit@(Allwit { staveware }) frame@(Speechframe { skale, speech }) = do
  let book = fst staveware
      sundries = sunder book skale speech
  writtens <- stavewriteAll wit (fromMaybe
    (flayLines $ map (writeSpeechOnFrame frame) (linewrap frame sundries))
    (_writings frame))
  return $ frame { _writings = Just writtens }

linewrap :: Speechframe -> [(String, Point)] -> [String]
linewrap frame@(Speechframe { nooks, rim }) sundries
  | length sundries < 2 = map fst sundries
  | otherwise = let
    -- todo: work this out
    Vertex2 w_speech _ = snd (head sundries)
    Vertex2 w_frame _ = (greatness nooks <-> Vertex2 (2*rim) 0)
    in if w_speech > w_frame
      then fst (head sundries) : linewrap frame (tail sundries)
      else linewrap frame (f (head sundries) (head $ tail sundries) : drop 2 sundries)
        where f x = bimap (fst x ++) (snd x <+>)

-- | Sunders the string into twains of each word with its bounding frame.
sunder :: Stavebook -> Scale -> String -> [(String, Point)]
sunder book scl = map ((second (stringframe book scl) . twin) . (++ " ")) . words

-- | Flays each line of writing below the last.
--
-- todo: draw out the @32@
flayLines :: [Writing] -> [Writing]
flayLines = zipWith (\i wr -> wr { _stead = Vertex2 0 (-32*i) <+> wr^.stead }) [0..]

-- | Fits the speech to the frame.
writeSpeechOnFrame :: Speechframe -> String -> Writing
writeSpeechOnFrame (Speechframe { skale, nooks, rim }) = Writing
  (West, North)
  skale
  white
  Nothing
  (topLeft nooks <+> Vertex2 rim -rim)

scaleToScreen :: Allwit -> Point -> Point
scaleToScreen (Allwit { display = RenderView { size = (w, h) } }) = (^*^) (Vertex2 (w/orwidth) (h/orheight))

allreckon :: Allwit -> Writing -> [Polyhedron]
allreckon allwit@(Allwit { staveware }) (Writing stk scl' _ _ std' wr) =
  let book = fst staveware
      advances = scanl (+) 0 (map (advance . (book!)) wr)
      (scl, std) = twimap (scaleToScreen allwit) (scl', std')
      offset = reckonStakes book stk scl wr
  in zipWith (\i char -> stavenook (offset <+> std) scl (advances!!i) (book!char)) [0..] wr

-- | The greater work.
stavewriteAll :: Stately a => Allwit -> [Writing] -> StateT a IO [Writing]
stavewriteAll allwit = (lift (useMesh (snd $ staveware allwit)) >>) . mapM (stavewrite allwit)

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

reckonStakes :: Stavebook -> Stakes -> Scale -> String -> Point
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
stringframe :: Stavebook -> Scale -> String -> Point
stringframe book scl = (scl ^*^) . foldr (yokeFrames . staveframe . (book!)) nonehood

-- | Yoke a twain of bounding frames.
yokeFrames :: Point -> Point -> Point
yokeFrames (Vertex2 x y) (Vertex2 a b) = Vertex2 (x+a) (max y b)

-- | Reckon the bounding frame of a stave.
staveframe :: Stave -> Point
staveframe (Stave (Vertex2 _ by) (Vertex2 _ _) step _) = Vertex2 step by

-- | Reckon the nooks of a stave.
stavenook :: Point -> Scale -> GLfloat -> Stave -> Polyhedron
stavenook bottomLeft scl step (Stave (Vertex2 left top) great@(Vertex2 _ height) _ _)
  = inject Z <$> fournook (z0 <+> z1) z0
    where
    z0 = bottomLeft <+> ((1/2) *^ scl ^*^ Vertex2 (left+step) (top-height))
    z1 = (1/2) *^ scl ^*^ great
