{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Synthetic where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Char (chr)
import System.IO
import Data.Function ((&))
import System.Random
import System.Process (
  callCommand,
  createProcess,
  CreateProcess(..),
  proc,
  shell,
  StdStream(..),
  waitForProcess,
  )

sineWavetable :: Int -> [Double]
sineWavetable (fromIntegral -> tableSize) = sin <$> period
  where
    period = do
      x <- [0..(tableSize - 1)]
      return $ x * 2 * pi / tableSize

squareWavetable :: Int -> [Double]
squareWavetable tableSize = replicate half 0.5 ++ replicate (tableSize - half) (-0.5)
  where
    half = tableSize `div` 2

sawWavetable :: Int -> [Double]
sawWavetable (fromIntegral -> tableSize) = (/tableSize) <$> [0..(tableSize -1)]

whiteWavetable :: Int -> [Double]
whiteWavetable (fromIntegral -> tableSize) = take tableSize whiteNoise
  where
    whiteNoise = snd . properFraction <$> randoms (mkStdGen 6)


-- decimator
-- good when n, the decimation ratio, is a positive integer
everyNth :: Int -> [a] -> [a]
everyNth 0 xs = error "n must be > 0"
everyNth n xs = everyNth' n 1 xs
  where
    everyNth' :: Int -> Int -> [a] -> [a]
    everyNth' _ _ [] = []
    everyNth' n 1 (x:xs) = x : everyNth' n n xs
    everyNth' n m (_:xs) = everyNth' n (m - 1) xs

-- resampler, now using linear interpolation
-- linearResample :: Int -> [a] -> [a]
-- linearResample = _

printSignal :: [Double] -> IO ()
printSignal = mapM_ $ \x ->
  let
    n = floor (x * size + size)
    size = 31 / 2
  in
    putStrLn $ replicate n ' ' ++ "."

type SampleRate = Int

tones :: SampleRate -> [Double] -> [(Double, Double)] -> [Double]
tones sr wavetable notes = mconcat (toSample <$> notes)
  where
    toSample (freq, dur) = signal sr freq wavetable
      & take (floor (dur * fromIntegral sr))

delaySecs :: SampleRate -> Double -> Double -> [Double] -> [Double]
delaySecs sr delay m sig = zipWith mix sig (drop numFrames sig)
  where
    numFrames = floor $ fromIntegral sr * delay
    mix a b = (1 - m) * a + m * b

midi2cps :: Double -> Double
midi2cps x = 2 ** ((x - 69)/12) * 440

signal :: SampleRate -> Double -> [Double] -> [Double]
signal sr freq wavetable = wavetable
  & cycle
  & everyNth flooredResampleRatio
  where
    flooredResampleRatio = floor freq * (length wavetable) `div` sr

quantize :: Double -> Int
quantize x = floor (x * size + size)
  where
    size = 255 / 2

-- aplay is by default unsigned 8 bit, 8000Hz
play :: [Double] -> IO ()
play list = do
  (Just h, _, _, _) <- createProcess (proc "aplay" []) { std_in = CreatePipe }
  hSetBuffering h NoBuffering
  hSetEncoding h char8
  mapM_ (hPutChar h . chr . quantize) list

-- A cheap livecode command
-- FIXME: don't use pkill, reduce scope of side effects :)
livecode :: [Double] -> IO ()
livecode list = do
  (_, _, _, p) <- createProcess (proc "pkill" ["aplay"]) { delegate_ctlc = True }
  waitForProcess p
  play list
