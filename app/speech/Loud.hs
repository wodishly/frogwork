{- HLINT ignore "Use section" -}
module Loud where

import Data.Char (toUpper)
import Data.Function (applyWhen)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import Mark (Branch, Mark (..), off, offs, ons, worth, worths)
import Token (betoken, shades)

import Mean (Shift, full, given, none)


type Loud = Branch Loudmark
type Flight = [Loud]

data Loudmark = Tung
          | Bear
            | Smooth | Nose
            | Throat | Stave | Spread | Clench
            | Long
          | Choke
            | Thru | Side | Step | Strong
            | Mouth | Lip | Ring | Blade | Far | Wide | Body | High | Fore | Back | Root | Low | Tight
  deriving (Eq)

instance Mark Loudmark where
  isAxled = flip elem [Tung, Bear, Smooth, Throat, Choke, Thru, Mouth]
  isSteadfast = flip elem [Bear, Choke, Thru]
  below' Tung = [Bear, Choke]
  below' Bear = [Smooth, Throat, Long]
  below' Smooth = [Nose]
  below' Throat = [Stave, Spread, Clench]
  below' Choke = [Thru, Mouth]
  below' Thru = [Side, Step, Strong]
  below' Mouth = [Lip, Blade, Body, Root]
  below' Lip = [Ring]
  below' Blade = [Far, Wide]
  below' Body = [High, Fore, Back]
  below' Root = [Low, Tight]
  below' _ = []

stillness :: String
stillness = "∅"

unstill :: Flight -> String
unstill ls = given full (cleans ls) stillness

meanBear :: Loud
meanBear = ons [Bear, Smooth, Stave, Thru, Body, Low] (def Tung)

meanChoke :: Loud
meanChoke = ons [Choke] (def Tung)

unloud :: Loud
unloud = off Tung (def Tung)

isGlide :: Loud -> Bool
isGlide x = none (flip worth x) [Bear, Choke]

isDerm :: Loud -> Bool
isDerm = worths [Bear, Choke]

isRough :: Loud -> Bool
isRough = not.worth Smooth

isThroat :: Loud -> Bool
isThroat l = all ($ l) [not.worth Root, worths [Thru, Body]]

unbear :: Shift Loud
unbear = offs [Bear, Long]

onbearThroat :: Shift Loud
onbearThroat = ons [Bear, Smooth, Stave] . offs [Step, Strong]

offbearThroat :: Shift Loud
offbearThroat = offs [Bear, Smooth, Throat] . ons [Step, Strong]

dirty :: String -> Loud
dirty c = fromMaybe (error $ "dirty of /" ++ c ++ "/ not found")
                    (lookup (fromMaybe c (lookup c shades)) bundles)

clean :: Loud -> String
clean l = fromMaybe (error $ "clean of " ++ show l ++ " not found")
                    (lookup l (map swap bundles))

dirtys :: String -> Flight
dirtys = map dirty . betoken

cleans :: Flight -> String
cleans = concatMap clean

instance Show Loudmark where
  show m = applyWhen (isAxled m) (map toUpper) $ case m of
    Tung -> "tung"
    Bear -> "bear"
    Smooth -> "smooth"
    Nose -> "nose"
    Throat -> "throat"
    Stave -> "stave"
    Spread -> "spread"
    Clench -> "clench"
    Choke -> "choke"
    Thru -> "thru"
    Side -> "side"
    Step -> "step"
    Strong -> "strong"
    Mouth -> "mouth"
    Lip -> "lip"
    Ring -> "ring"
    Blade -> "blade"
    Far -> "far"
    Wide -> "wide"
    Body -> "body"
    High -> "high"
    Fore -> "fore"
    Back -> "back"
    Root -> "root"
    Low -> "low"
    Tight -> "tight"
    Long -> "long"

-- reach with `clean` and `dirty`
bundles :: [(String, Loud)]
bundles = [
   ("ʔ", meanChoke)

 , ("p", ons [Lip] $ dirty "ʔ")
 , ("t", ons [Blade] $ dirty "ʔ")
 , ("k", ons [High] $ dirty "ʔ")
 , ("kʸ", ons [Fore] $ dirty "k")
 , ("kʷ", ons [Ring] $ dirty "k")

 , ("b", ons [Stave] $ dirty "p")
 , ("d", ons [Stave] $ dirty "t")
 , ("g", ons [Stave] $ dirty "k")
 , ("gʸ", ons [Fore] $ dirty "g")
 , ("gʷ", ons [Ring] $ dirty "g")

 , ("bʰ", ons [Spread] $ dirty "b")
 , ("dʰ", ons [Spread] $ dirty "d")
 , ("gʰ", ons [Spread] $ dirty "g")
 , ("gʸʰ", ons [Spread, Fore] $ dirty "g")
 , ("gʷʰ", ons [Spread, Ring] $ dirty "g")

 , ("f", ons [Thru, Step] $ dirty "p")
 , ("θ", ons [Thru, Step] $ dirty "t")
 , ("ð", ons [Stave] $ dirty "θ")

 , ("s", ons [Thru, Step, Strong] $ dirty "t")
 , ("z", ons [Stave] $ dirty "s")
 , ("ɬ", ons [Thru, Step, Side] $ dirty "t")
 , ("ɮ", ons [Stave] $ dirty "ɬ")
 , ("h₂", ons [Body] $ offs [Blade] $ dirty "s")
 , ("h₁", ons [Fore] $ dirty "h₂")
 , ("h₃", ons [Ring] $ dirty "h₂")
 , ("h", ons [Thru, Step] $ dirty "ʔ")

 , ("m", ons [Nose] $ dirty "b")
 , ("n", ons [Nose] $ dirty "d")
 , ("ŋ", ons [Nose] $ dirty "g")
 , ("ŋʷ", ons [Nose] $ dirty "gʷ")
 , ("l", ons [Side] $ dirty "r")
 , ("r", ons [Thru] $ offs [Nose] $ dirty "n")
 , ("y", offs [Bear] $ dirty "i")
 , ("w", offs [Bear] $ dirty "u")

 , ("ə₂", onbearThroat $ dirty "h₂")
 , ("ə₁", onbearThroat $ dirty "h₁")
 , ("ə₃", onbearThroat $ dirty "h₃")
 , ("m̩", ons [Bear] $ dirty "m")
 , ("n̩", ons [Bear] $ dirty "n")
 , ("l̩", ons [Bear] $ dirty "l")
 , ("r̩", ons [Bear] $ dirty "r")
 , ("i", ons [High] $ offs [Low] $ dirty "e")
 , ("u", ons [High] $ offs [Low] $ dirty "o")

 , ("e", ons [      Fore, Tight] $ offs [Low] $ dirty "a")
 , ("o", ons [Ring, Back, Tight] $ offs [Low] $ dirty "a")
 , ("a", meanBear)

 , ("m̩̄", ons [Long] $ dirty "m̩")
 , ("n̩̄", ons [Long] $ dirty "n̩")
 , ("l̩̄", ons [Long] $ dirty "l̩")
 , ("r̩̄", ons [Long] $ dirty "r̩")
 , ("ī", ons [Long] $ dirty "i")
 , ("ū", ons [Long] $ dirty "u")
 , ("ē", ons [Long] $ dirty "e")
 , ("ō", ons [Long] $ dirty "o")
 , ("ā", ons [Long] $ dirty "a")
 ]