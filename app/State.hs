module State (
  News
, Settings (..)
, StateName (..)
, Stately (..)
, isRunningTests
, isShowingKeys
, isShowingTicks
, makeSettings
, preent
) where

import Control.Lens (makeLenses)
import Control.Monad.State (StateT, MonadTrans (lift))

import Happen (Mousewit)
import Key (Keyset)
import Matrix (RenderView)
import Stavemake (Staveware)
import Time (Time)


type News = (Keyset, Mousewit, RenderView, Time)

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
