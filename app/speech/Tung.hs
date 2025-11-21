module Tung (
) where


-- type FrogWord = [Breath]

-- loudhoard :: [Loud]
-- loudhoard = map dirty [
--     "p", "t",  "k", "kw",  "q"
--   , "m", "n", "ng"
--   , "u", "i",  "a",  "e",  "o"
--   ]

-- rloud' :: (Loud -> Bool) -> IO Loud
-- rloud' ordeal = (ls !!) . floor . ((*).fromIntegral.length) ls <$> rand
--   where ls = filter ordeal loudhoard

-- rloud :: IO Loud
-- rloud = rloud' (const True)

-- rbear :: IO Loud
-- rbear = rloud' (worth Bear)

-- rchoke :: IO Loud
-- rchoke = rloud' (worth Choke)

-- ronset :: IO Flight
-- ronset = shell <$> rchoke

-- rinset :: IO Flight
-- rinset = shell <$> rbear

-- roffset :: IO Flight
-- roffset = shell <$> rchoke

-- rrime :: IO Rime
-- rrime = Rime <$> rinset <*> roffset

-- rbreath :: IO Breath
-- rbreath = Breath <$> ronset <*> rrime <*> pure False

-- rword :: IO FrogWord
-- rword = replicate 2 <$> rbreath

-- rword :: IO FrogWord
-- rword = rword' 0
-- 
-- rword' :: Int -> IO FrogWord
-- rword' n = rand >>= \r -> (:) <$> rbreath <*>
--   if r < 1/cast n then rword' (succ n) else pure []