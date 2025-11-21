module AboutState (
  AboutState (..),
  makeAboutState,
  writings
) where

import Control.Lens (makeLenses)
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT)

import Graphics.Rendering.OpenGL (Vertex2 (Vertex2))

import Allwit (Allwit (timewit))
import State (StateName (AboutName), Stately (..), doAtEach)

import Blee (bg, darkwhelk)
import FastenMain (orheight, orwidth)
import Random (rand)
import Rime (Point)
import Stavework (Writing (_stead), makeWriting, renderFeather, stavewriteAll)


newtype AboutState = AboutState {
  _writings :: [Writing]
}
makeLenses ''AboutState

instance Stately AboutState where
  name _ = AboutName

  update allwit = do
    doAtEach (timewit allwit) 0.5 flutter
    return allwit

  render allwit = do
    aboutwit <- get
    bg darkwhelk
    renderFeather allwit
    ws <- stavewriteAll allwit (_writings aboutwit)
    put aboutwit { _writings = ws }

makeAboutState :: Point -> AboutState
makeAboutState wind = AboutState [makeWriting wind "rɪbɪt"]

flutter :: StateT AboutState IO ()
flutter = do
  aboutwit@AboutState { _writings } <- get
  rx <- lift rand
  ry <- lift rand
  put aboutwit { _writings = [(head _writings) { _stead = Vertex2 (rx*orwidth) (ry*orheight) }] }
