module AboutState (
  AboutState (..)
, makeAboutState
) where

import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT)

import Graphics.Rendering.OpenGL (Vertex2 (Vertex2))

import State (StateName (AboutName), Stately (..), doAtEach)

import Allwit (Allwit (timewit))
import Blee (bg, darkwhelk)
import FastenMain (orheight, orwidth)
import Random (rand)
import Rime (Point)
import Stavework (Writing (_stead), makeWriting, renderFeather, stavewriteAll)


newtype AboutState = AboutState {
  writings :: [Writing]
}

instance Stately AboutState where
  name _ = AboutName

  update allwit = do
    doAtEach (timewit allwit) 0.5 flutter
    return allwit

  render allwit = do
    aboutwit <- get
    bg darkwhelk
    renderFeather allwit
    ws <- stavewriteAll allwit (writings aboutwit)
    put aboutwit { writings = ws }

makeAboutState :: Point -> AboutState
makeAboutState wind = AboutState [makeWriting wind "rɪbɪt"]

flutter :: StateT AboutState IO ()
flutter = do
  aboutwit <- get
  rx <- lift rand
  ry <- lift rand
  put aboutwit { writings = [(head (writings aboutwit)) { _stead = Vertex2 (rx*orwidth) (ry*orheight) }] }
