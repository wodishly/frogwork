module State (
  News
, Settings (..)
, StateName (..)
, Stately (..)
, isRunningTests
, isShowingKeys
, isShowingTicks
, makeSettings
, toggle
, preent
, doAt
) where

import Control.Lens (Lens', makeLenses, (.~), (^.))
import Control.Monad (when)
import Control.Monad.State (MonadTrans (lift), StateT)

import Happen (Mousewit)
import Key (Keyset)
import Matrix (RenderView)
import Mean (between)
import Stavemake (Staveware)
import Time (Timewit (..))


type News = (Keyset, Mousewit, RenderView, Timewit)

data StateName
  = TitleName
  | WillName
  | PlayName
  | PauseName
  | EndName
  deriving (Show, Eq, Ord)

data Settings = Settings {
  _isShowingTicks :: Bool
, _isShowingKeys :: Bool
, _isRunningTests :: Bool
} deriving (Show, Eq)
makeLenses ''Settings

makeSettings :: Settings
makeSettings = Settings False False False

toggle :: Lens' Settings Bool -> Settings -> Settings
toggle lens settings = (lens.~not (settings^.lens)) settings

class Stately a where
  name :: a -> StateName
  staveware :: a -> Staveware

  update :: News -> StateT a IO ()
  render :: News -> StateT a IO ()

  loop :: News -> StateT a IO ()
  loop news = do
    update news
    render news

-- | Curse this not with `(Stately b) =>`, lest @preent@ no longer become @Allwit@.
preent :: Show a => a -> StateT b IO ()
preent = lift . print

doAt :: (RealFrac a, Stately b) => Timewit -> a -> StateT b IO () -> StateT b IO ()
doAt time t = when $ between (lifetime time, lifetime time + delta time) (round t)
