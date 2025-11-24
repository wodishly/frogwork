module Loudness where

import Data.Vector.Storable.Mutable

import SDL.Audio
import FastenMain


type Loudness = (AudioDevice, AudioSpec)

spoken :: IO Loudness
spoken = openAudioDevice stillness { openDeviceCallback = callback } >>= ordeal >>= (print "i awaken" >>) . return

bestill :: Loudness -> IO ()
bestill (loudwile, _) = print "i sleep" >> closeAudioDevice loudwile

rest :: AudioDevice -> IO ()
rest wile = print "i rest" >> setAudioDevicePlaybackState wile Pause

unrest :: AudioDevice -> IO ()
unrest wile = print "i unrest" >> setAudioDevicePlaybackState wile Play

ordeal :: Loudness -> IO Loudness
ordeal loudness@(_, AudioSpec {
  audioSpecFreq,
  audioSpecFormat,
  audioSpecChannels,
  audioSpecSilence,
  audioSpecSamples,
  audioSpecSize
}) = do
  getAudioDrivers >>= print . (audioDriverName <$>)
  currentAudioDriver >>= print
  print audioSpecFreq
  print audioSpecFormat
  print audioSpecChannels
  print audioSpecSilence
  print audioSpecSamples
  print audioSpecSize
  -- pauseAudio 0
  return loudness

callback :: AudioFormat a -> IOVector a -> IO ()
callback format _vector = do
  -- M.write _vector 0 1
  -- x <- set _vector =<< _vector
  print $ "whee" ++ show format
  return ()
