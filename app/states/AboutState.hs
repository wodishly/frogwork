module AboutState (
  AboutState (..)
, makeAboutState
) where

import Control.Monad.State (MonadState(get, put), StateT, MonadTrans (lift))

import State (StateName (AboutName), Stately (..), doEvery)

import Blee (bg, darkwhelk)
import Rime (Point, (*^))
import Stavework (Writing (stead), makeWriting, renderFeather, stavewrite)
import Random (rand)
import FastenMain (orheight, orwidth)
import Graphics.Rendering.OpenGL (Vertex2(Vertex2))
import Allwit (Allwit(timewit))


newtype AboutState = AboutState {
  writings :: [Writing]
}

instance Stately AboutState where
  name _ = AboutName

  update allwit = do
    doEvery (timewit allwit) 0.5 flutter
    return allwit

  render allwit = do
    aboutwit <- get
    bg darkwhelk
    renderFeather allwit
    stavewrite allwit (writings aboutwit)

makeAboutState :: Point -> IO AboutState
makeAboutState wind = rand >>= \r -> return $ AboutState [makeWriting (r *^ wind) "rɪbɪt"]

flutter :: StateT AboutState IO ()
flutter = do
  aboutwit <- get
  rx <- lift rand
  ry <- lift rand
  put aboutwit { writings = [(head (writings aboutwit)) { stead = Vertex2 (rx*orwidth) (ry*orheight) }] }