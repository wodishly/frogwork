module State where

import Data.HashMap.Lazy (member, (!))

import Allwit
import Matrix hiding ((!))
import Mean
import Rime
import Shade
import Stavework
import Time


data StateName
  = TitleName
  | WillName
  | PlayName
  | PauseName
  | AboutName
  | EndName
  deriving (Show, Eq, Ord)

class Stately a where
  name :: a -> StateName

  update :: Allwit -> StateT a IO Allwit
  update = return

  render :: Allwit -> StateT a IO Allwit
  render = return

  loop :: Allwit -> StateT a IO Allwit
  loop allwit = update allwit >>= render

  stavewrite :: Lens' a [Writing] -> Allwit -> StateT a IO ()
  stavewrite lens allwit = do
    statewit <- get
    renderFeather allwit
    ws <- stavewriteAll allwit (statewit^.lens)
    put $ (lens.~ws) statewit

doOnceAt :: Stately a => Float -> Timewit -> StateT a IO () -> StateT a IO ()
doOnceAt t Timewit { lifetime, delta } = when (between (lifetime, lifetime+delta) t)

doAtEach :: Stately a => Float -> Timewit -> StateT a IO () -> StateT a IO ()
doAtEach t Timewit { lifetime, delta } = when (mod' lifetime t > mod' (lifetime + delta) t)

-- | The greater work.
stavewriteAll :: Stately a => Allwit -> [Writing] -> StateT a IO [Writing]
stavewriteAll allwit@Allwit { staveware = (_, m) } = (lift (useMesh m) >>) . mapM (stavewrite' allwit)

-- | The great work.
stavewrite' :: Stately a => Allwit -> Writing -> StateT a IO Writing
stavewrite' allwit@(Allwit { staveware = (book, mish) }) writing@Writing { _throoks, _blee, writ } = do
  let newrooks = Just (fromMaybe (allreckon allwit writing) _throoks)
  forM_ (zip [0..] writ) $ \(i, char) ->
    if member char book
    then lift $ carve (book!char) (fromJust newrooks!!i) mish _blee
    else error $ show char ++ " is not in the book"

  return writing { _throoks = newrooks }

drawWith :: Allwit -> FrogMatrix -> Mesh -> IO ()
drawWith Allwit { display, timewit } = drawMesh
  (getPerspectiveMatrix display)
  (getOrthographicMatrix display)
  timewit

renderFeather :: Stately a => Allwit -> StateT a IO ()
renderFeather allwit@Allwit { staveware } = lift $ drawWith allwit (ident 4) (snd staveware)
