module Tung where

import Tung.Breath
import Tung.Loud
import Tung.Shift ()
import Tung.Token ()
import Tung.Mark
import Random (rand)
import Mean (shell, flight, allIn)
import Data.Bool (bool)
import Data.Maybe (maybeToList)

orcroak :: IO ()
orcroak = putStrLn "rɪbɪt"

fremdcroak :: IO ()
fremdcroak = croak >>= putStrLn . cleans . flatten

croak :: IO FrogWord
croak = rFrogWord

type FrogWord = [Breath]

loudhoard :: [Loud]
loudhoard = map dirty [
  "p", "t",  "k", "kw",  "q",
  "m", "n", "ng",
  "l", "r",
  "u", "i",  "a",  "e",  "o"
  ]

rloudOf :: (Loud -> Bool) -> IO Loud
rloudOf ordeal = (ls !!) . floor . ((*) . fromIntegral . length) ls <$> rand
  where ls = filter ordeal loudhoard

rloud :: IO Loud
rloud = rloudOf (const True)

rMaybeLoud :: Real a => a -> IO (Maybe Loud)
rMaybeLoud = rMaybe rloud

rMaybe :: Real a => IO b -> a -> IO (Maybe b)
rMaybe f = (rand >>=) . (bool (pure Nothing) (Just <$> f) .) . (>) . realToFrac

rbear :: IO Loud
rbear = rloudOf (worth Bear)

rchoke :: IO Loud
rchoke = rloudOf (worth Choke)

rside :: IO Loud
rside = rloudOf (allIn [worths [Smooth, Thru], not . worth Nose, not . worth Bear])

ronset :: IO Flight
ronset = shell <$> rchoke

rinset :: IO Flight
rinset = shell <$> rbear

roffset :: IO Flight
roffset = shell <$> rchoke

rrime :: IO Rime
rrime = Rime <$> rinset <*> roffset

rbreath :: IO Breath
rbreath = Breath <$> ronset <*> rrime <*> pure False

rword :: IO FrogWord
rword = traverse (const rbreath) (flight 2)

rFrogOnset :: IO Flight
rFrogOnset = (:) <$> rloudOf isRough <*> (maybeToList <$> rMaybe rside 0.5)

rFrogBreath :: IO Breath
rFrogBreath = Breath <$> rFrogOnset <*> rFrogRime <*> pure False

rFrogRime :: IO Rime
rFrogRime = Rime <$> rinset <*> rFrogOffset

rFrogOffset :: IO Flight
rFrogOffset = shell <$> rloudOf isRough

rFrogWord :: IO FrogWord
rFrogWord = mapM (<*> pure False) [
  Breath <$> rFrogOnset <*> (Rime <$> rinset <*> (shell <$> rloudOf (allIn [worth Smooth, not . worth Bear]))),
  Breath <$> rFrogOnset <*> (Rime <$> rinset <*> (shell <$> rloudOf isRough))
  ]

rwordZipf :: Int -> IO FrogWord
rwordZipf n = rand >>= \r -> (:) <$> rbreath <*>
  if r < 1/fromIntegral n then rwordZipf (succ n) else pure []
