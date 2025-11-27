module AboutState where

import Allwit
import Blee
import FastenMain
import Mean
import Random
import Rime
import State
import Stavework


newtype AboutState = AboutState {
  _writings :: [Writing]
}
makeLenses ''AboutState

instance Stately AboutState where
  name _ = AboutName

  update allwit = do
    doAtEach 0.5 (timewit allwit) flutter
    return allwit

  render allwit = do
    aboutwit <- get
    bg darkwhelk
    renderFeather allwit
    ws <- stavewriteAll allwit (_writings aboutwit)
    put aboutwit { _writings = ws }
    return allwit

makeAboutState :: Point -> AboutState
makeAboutState wind = AboutState [makeWriting wind "rɪbɪt"]

flutter :: StateT AboutState IO ()
flutter = do
  aboutwit@AboutState { _writings } <- get
  rx <- lift rand
  ry <- lift rand
  preent "hi"
  put aboutwit { _writings = [(head _writings) { stead = Vertex2 (rx*orwidth) (ry*orheight) }] }
