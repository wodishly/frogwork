module Loudness
  ( Loudness,
    spoken,
    bestill,
    loud,
  )
where

import SDL.Audio
  ( AudioDevice,
    AudioSpec (..),
    audioDriverName,
    closeAudioDevice,
    currentAudioDriver,
    getAudioDrivers,
    openAudioDevice, AudioFormat, OpenDeviceSpec (openDeviceCallback),
  )

import FastenMain (stillness)
import SDL.Raw.Audio (pauseAudio)
import Data.Vector.Storable.Mutable (IOVector)


type Loudness = (AudioDevice, AudioSpec)

spoken :: IO Loudness
spoken = openAudioDevice stillness { openDeviceCallback = loud } >>= ordeal >>= (print "i awaken" >>) . return

bestill :: Loudness -> IO ()
bestill (loudwile, _) = print "i sleep" >> closeAudioDevice loudwile

ordeal :: Loudness -> IO Loudness
ordeal loudness@(_, AudioSpec {
  audioSpecFreq,
  audioSpecFormat,
  audioSpecChannels,
  audioSpecSilence,
  audioSpecSamples,
  audioSpecSize
}) =
  -- let wit = audioSpecFormat
  getAudioDrivers
  >>= print . (audioDriverName <$>)
  >> currentAudioDriver
  >>= print
  >> print audioSpecFreq
  >> print audioSpecFormat
  >> print audioSpecChannels
  >> print audioSpecSilence
  >> print audioSpecSamples
  >> print audioSpecSize
  >> pauseAudio 0
  >> return loudness

loud :: AudioFormat a -> IOVector a -> IO ()
loud _format _vector = do
  return ()
