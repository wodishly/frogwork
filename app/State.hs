module State where

import Data.HashMap.Lazy (member, (!))

import Allwit
import Matrix hiding ((!))
import Mean
import Rime
import Shade
import Stavework
import Time
import Blee
import Key


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

  stave :: Lens' a [Writing] -> Allwit -> StateT a IO ()
  stave lens allwit = do
    statewit <- get
    renderFeather allwit
    ws <- stavewriteAll allwit (statewit^.lens)
    put $ (lens.~ws) statewit

class Stately a => Choosing a b | a -> b where
  chosen :: a -> b
  updateHand :: Allwit -> StateT a IO Allwit

  showFinger :: Stately a => Lens' a [Writing] -> Lens' a Int -> StateT a IO ()
  showFinger writings finger = do
    statewit <- get
    put $ (over writings $ choose statewit . unchoose) statewit
    where choose statewit = hit (set blee red) (succ (view finger statewit))
          unchoose = map (set blee lightwhelk)

  nudgeFinger :: Allwit -> Lens' a Int -> Lens' a [b] -> StateT a IO ()
  nudgeFinger Allwit { keyset } finger hand = do
    statewit <- get
    let nudge
          | keyBegun keyset ScancodeUp = pred
          | keyBegun keyset ScancodeDown = succ
          | otherwise = id
    put $ (set finger $ mod (nudge (view finger statewit)) (length (view hand statewit))) statewit

doOnceAt :: Stately a => Float -> Timewit -> StateT a IO () -> StateT a IO ()
doOnceAt t Timewit { lifetime, delta } = when (between (lifetime, lifetime+delta) t)

doAtEach :: Stately a => Float -> Timewit -> StateT a IO () -> StateT a IO ()
doAtEach t Timewit { lifetime, delta } = when (mod' lifetime t > mod' (lifetime+delta) t)

-- | The greater work.
stavewriteAll :: Stately a => Allwit -> [Writing] -> StateT a IO [Writing]
stavewriteAll allwit@Allwit { meshhoard = Meshhoard { stavemesh } } = (lift (useMesh stavemesh) >>) . mapM (stavewrite' allwit)

-- | The great work.
stavewrite' :: Stately a => Allwit -> Writing -> StateT a IO Writing
stavewrite' allwit@(Allwit { stavebook, meshhoard = Meshhoard { stavemesh } }) writing@Writing { _throoks, _blee, writ } = do
  let newrooks = Just (fromMaybe (allreckon allwit writing) _throoks)
  forM_ (zip [0..] writ) $ \(i, char) ->
    if member char stavebook
    then lift $ carve (stavebook!char) (fromJust newrooks!!i) stavemesh _blee
    else error $ show char ++ " is not in the book"

  return writing { _throoks = newrooks }

drawWith :: Allwit -> FrogMatrix -> Mesh -> IO ()
drawWith Allwit { display, timewit } = drawMesh
  (getPerspectiveMatrix display)
  (getOrthographicMatrix display)
  timewit

renderFeather :: Stately a => Allwit -> StateT a IO ()
renderFeather allwit@Allwit { meshhoard = Meshhoard { stavemesh } } = lift $ drawWith allwit (ident 4) stavemesh
