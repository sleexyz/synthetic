{-# LANGUAGE RankNTypes #-}

import Sound.Synthetic
import Control.Monad (ap, join)
import Data.List
import Data.Bifunctor
import Prelude hiding (($))

infixr 1 $
($) :: (a -> b) -> a -> b
f $ x = f x

infixl 0 &
(&) :: a -> (a -> b) -> b
x & f = f x

-- ap transposed
apT :: (Monad m) => m (a -> b) -> m a -> m b
apT f x = fmap (&) x `ap` f


main :: IO ()
main = livecode $ stream

speed :: (Functor f, Functor g) => Float -> f (g Float)  -> f (g Float)
speed n = fmap . fmap $ (/n)

fit :: (Functor g) => ([g Float -> g Float] -> [g Float] -> [g Float]) -> [g Float -> g Float] -> [g Float] -> [g Float]
fit ap' xs = speed (fromIntegral $ length xs) . ap' xs

mix :: Float -> ([Float] -> [Float]) -> [Float] -> [Float]
mix p f x  = zipWith (+) (f x & fmap (*p)) (x & fmap (*(1 - p)))

sr = 48000

white = (whiteWavetable sr & cycle)

stream :: [Float]
stream = tones sr table tonePairs
  & mix 0.25 $ zipWith (*) (cycle ([0, 1] >>= replicate (2^4)))
  & fmap (tanh)
  & mix 0.125 $ drop (sr * 3 `div` 10)
  & mix 0.5 $ (zipWith (*) $ ((sineWavetable (sr * 8))
                    & fmap (/2) 
                    & fmap (+0.5) 
                    & cycle 
                    & mix 0.5 $ const white
                 ))
  where
    table = []
      & mappend (squareWavetable sr)
      & fmap (tanh)
      & fmap (tanh)
      & fmap (tanh)

    tonePairs = (fmap . first) midi2cps notePairs

    notePairs = [(78, 1/2)]
      & fit ap (first . flip (+) <$> [0, -12, -19, -96, -28])
      & fit ap $ first . (+) <$> [24, 12, 0, -12]
      & ap $ first . (+) <$> [0, 12, 24]
      & ap $ first . (+) <$> [0, -5, -7, -2]
      & ap $ first . (+) <$> [0, 7]
      & ap $ first . (+) <$> [0, -5]
      & ap $ first . (+) <$> [-96, 0, 24, 0, 24, 0, 12, 0, 24 ]
      & cycle
      -- & zipWith ($) $ first . (+) <$> cycle ([24, -128] >>= replicate 1)
      -- & zipWith ($) $ first . ($) <$> cycle ([id,  (subtract 24), id] >>= replicate 1)
      -- & zipWith ($) $ first . ($) <$> cycle ([id, id, (subtract 24)] >>= replicate 1)
      -- & zipWith ($) $ second . (*) <$> cycle ([1, 0, 1, 0, 0, 0] >>= replicate 1)
