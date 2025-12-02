module AboutState where

import Blee
import Mean
import Rime
import State
import Stavework
import Allwit
import FastenMain


newtype AboutState = AboutState {
  _writings :: [Writing]
}
makeLenses ''AboutState

instance Stately AboutState where
  name _ = AboutName

  update allwit = do
    let (ws, allwit') = weirds 2 allwit
    doAtEach 1 (timewit allwit) (flutter ws)
    return allwit'

  render allwit = do
    bg darkwhelk
    stave writings allwit
    return allwit

makeAboutState :: Point -> AboutState
makeAboutState wind = AboutState [makeWriting wind "rɪbɪt"]

flutter :: [Float] -> StateT AboutState IO ()
flutter ws = do
  aboutwit@AboutState { _writings } <- get
  let x = head _writings
      z = Vertex2 (head ws*orwidth) (head (tail ws)*orheight)
  put aboutwit { _writings = [flush $ x { stead = z }] }
  preent $ aboutwit^.writings
