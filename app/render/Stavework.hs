{-|
@Stavemake@ is for anything to do with writing the stave.
@Stavework@ imports @State@, so @State@ must not import @Stavework@.
-}
module Stavework where

import Data.HashMap.Lazy ((!), member)

import Allwit
import Blee
import FastenMain
import Matrix hiding ((!))
import Mean
import Rime
import Shade
import State
import Stavemake


type Scale = Point

data Writing = Writing {
  stakes :: Stakes,
  scale :: Scale,
  _blee :: Blee,
  _throoks :: Maybe [Polyhedron],
  stead :: Point,
  writ :: String
}
makeLenses ''Writing

-- | Twimiddle, truescale, lightwhelk, unwone.
-- Otherwise brook the orshapework @Writing@.
makeWriting :: Point -> String -> Writing
makeWriting = Writing (Middle, Middle) onehood lightwhelk Nothing

data Speechframe = Speechframe {
  rim :: GLfloat,
  scale :: Scale,
  writtens :: Maybe [Writing],
  nooks :: Fournook,
  meesh :: Mesh,
  string :: String
}
makeLenses ''Speechframe

makeSpeechframe :: Mesh -> String -> Speechframe
makeSpeechframe = Speechframe 22.5 ((1/3) *^ onehood) Nothing
  (Fournook (orwest <+> Vertex2 50 -75) (orwindow <-> Vertex2 100 50))

speechwrite :: Stately a => Allwit -> Speechframe -> StateT a IO Speechframe
speechwrite wit@(Allwit { staveware = (book, _) }) frame@(Speechframe { scale, string, writtens }) = do
  let sundries = sunder book scale string
  ws <- stavewriteAll wit (fromMaybe
    (flayLines $ map (writeSpeechOnFrame frame) (linewrap frame sundries))
    writtens)
  return $ frame { writtens = Just ws }

becwethe :: Uniform p => Mesh -> String -> p -> IO ()
becwethe meshful s value = uniformMap meshful ! s >>= ($= value) . uniform

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
flayLines = zipWith (\i wr -> wr { stead = Vertex2 0 (-32*i) <+> stead wr }) [0..]

-- | Fits the speech to the frame.
writeSpeechOnFrame :: Speechframe -> String -> Writing
writeSpeechOnFrame (Speechframe { scale, nooks, rim }) = Writing
  (West, North)
  scale
  white
  Nothing
  (topLeft nooks <+> Vertex2 rim -rim)

scaleToScreen :: Allwit -> Point -> Point
scaleToScreen (Allwit { display = RenderView { size = (w, h) } }) = (^*^) (Vertex2 (w/orwidth) (h/orheight))

allreckon :: Allwit -> Writing -> [Polyhedron]
allreckon allwit@(Allwit { staveware = (book, _) }) (Writing stk scl' _ _ std' wr) =
  let advances = scanl (+) 0 (map (advance . (book!)) wr)
      (scl, std) = twimap (scaleToScreen allwit) (scl', std')
      offset = reckonStakes book stk scl wr
  in zipWith (\i char -> stavenook (offset <+> std) scl (advances!!i) (book!char)) [0..] wr

-- | The greater work.
stavewriteAll :: Stately a => Allwit -> [Writing] -> StateT a IO [Writing]
stavewriteAll allwit@Allwit { staveware = (_, m) } = (lift (useMesh m) >>) . mapM (stavewrite allwit)

-- | The great work.
stavewrite :: Stately a => Allwit -> Writing -> StateT a IO Writing
stavewrite allwit@(Allwit { staveware = (book, mish) }) writing@Writing { _throoks, _blee, writ } = do
  let newrooks = Just (fromMaybe (allreckon allwit writing) _throoks)
  forM_ (zip [0..] writ) $ \(i, char) ->
    if member char book
    then lift $ carve (book!char) (fromJust newrooks!!i) mish _blee
    else error $ show char ++ " is not in the book"

  return writing { _throoks = newrooks }

-- | Carves the staves into being.
carve :: Stave -> Polyhedron -> Mesh -> Blee -> IO ()
carve (Stave { texture }) vertices mish@(Mesh { vbo, elementCount }) bl = do
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just texture
  bindBuffer ArrayBuffer $= Just vbo
  becwethe mish "u_blee" (bleeToGLVector4 bl)
  withArray vertices (bufferSubData ArrayBuffer WriteToBuffer 0 $ bufferSize vertices)
  bindBuffer ArrayBuffer $= Nothing
  drawFaces elementCount

renderFeather :: Stately a => Allwit -> StateT a IO ()
renderFeather (Allwit { display, timewit, staveware }) = lift $ drawMesh
  (getPerspectiveMatrix display)
  (ident 4)
  (getOrthographicMatrix display)
  timewit
  (snd staveware)

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
