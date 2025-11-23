module Loudness
  ( Loudness,
    spoken,
    bestill,
    rest,
    unrest
  )
where

import Data.Vector.Storable.Mutable (MVector, RealWorld, unsafeRead)

import SDL.Audio
  ( AudioDevice,
    AudioFormat,
    AudioSpec (..),
    OpenDeviceSpec (openDeviceCallback),
    PlaybackState (Pause, Play),
    audioDriverName,
    closeAudioDevice,
    currentAudioDriver,
    getAudioDrivers,
    openAudioDevice,
    setAudioDevicePlaybackState,
  )

import FastenMain (stillness)


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

callback :: AudioFormat a -> MVector RealWorld a -> IO ()
callback format vector = do
  -- print $ sizeOf vector
  print $ "whee" ++ show format
  -- print $ unsafeRead 0 vector
  return ()
