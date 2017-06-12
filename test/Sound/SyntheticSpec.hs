{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Sound.SyntheticSpec where

import Test.Hspec
import Test.QuickCheck
import Numeric.Natural (Natural)
import Sound.Synthetic


spec :: Spec
spec = do
  describe "Oscillator" $ do
    describe "Wavetables" $ do
      let
        testWaveTableGen :: String -> (Int -> [Float]) -> Spec
        testWaveTableGen name wavetableGen = do
          describe name $ do
            it "preserves length" $ property $ \(fromIntegral @ Natural -> n) ->
              length (wavetableGen n) == n

            it "is between -1 and 1" $ property $ \(fromIntegral @ Natural -> n) ->
              and $ (\x -> (x <= 1) && (x >= -1)) <$> wavetableGen n

      testWaveTableGen "sineWavetable" sineWavetable
      testWaveTableGen "squareWavetable" squareWavetable
      testWaveTableGen "sawWavetable" sawWavetable
      testWaveTableGen "whiteWavetable" whiteWavetable

    describe "midi2cps" $ do
      it "sends 69 to 440" $ do
        midi2cps 69 `shouldBe` 440

    describe "linearResampler" $ it "" pending

    describe "signal" $ do
      it "can modulate at any positive real frequency" $ pending

    describe "quantize" $ do
      it "maps -1 to 0" $ do
        quantize (-1) `shouldBe` 0

      it "maps 1 to 255" $ do
        quantize 1 `shouldBe` 255

    describe "convolution based reverb" $ it "" pending
