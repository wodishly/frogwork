{-# LANGUAGE GADTs #-}
module Loudness where

import SDL.Audio
import FastenMain
import qualified Data.Vector.Storable.Mutable as M
import Control.Monad
import Foreign
import Data.IORef
import Mean


type Loudness = (AudioDevice, AudioSpec)

bestill :: Loudness -> IO ()
bestill (loudwile, _) = print "i sleep" >> closeAudioDevice loudwile

rest :: AudioDevice -> IO ()
rest wile = print "i rest" >> setAudioDevicePlaybackState wile Pause

unrest :: AudioDevice -> IO ()
unrest _wile = return () -- print "i unrest" >> setAudioDevicePlaybackState wile Play

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
  return loudness

stream :: [Int8]
stream =
  -- map (\n -> round (fromIntegral (div maxBound 2 :: Int8) * sin (2*pi * 440 * fromIntegral n / 44100)))
  map (round . (fromIntegral (div maxBound 2 :: Int8) *) . sin . (2*pi * 440 *) . (/ 44100) . fromIntegral)
      [0 :: Int16 ..]

spoken :: IO Loudness
spoken = do
  waves <- newIORef stream
  loudness <- ordeal =<< openAudioDevice stillness { openDeviceCallback = callback waves }
  print "i awaken"
  return loudness

callback :: IORef [Int8] -> AudioFormat a -> M.IOVector a -> IO ()
callback waves format buffer = case ly format of
  Signed8BitAudio -> do
    waves' <- ly <$> readIORef waves
    print $ "waves'" ++ show waves'
    let n = M.length buffer
    print $ "n" ++ show n
    zipWithM_ (M.write buffer) [0..] (take n waves')
    print "zipped"
    writeIORef waves (drop n waves')
    print "written"
  _ -> error "no"
