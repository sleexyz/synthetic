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

stream :: [Float]
stream = tones 8000 table tonePairs
  & zipWith (*) (sineWavetable 32000  & fmap (/3) & fmap (+0.5) & cycle)
  & mix 0.5 $ zipWith (*) (cycle [0, 1])
  & mix 0.5 $ drop (6000)
  where
    table = []
      & mappend (sineWavetable 8000)
      & fmap (tanh)

    tonePairs = (fmap . first) midi2cps notePairs

    notePairs = [(64, 1/2)]
      & fit ap $ first . (+) <$> [0, -12]
      & fit ap $ first . (+) <$> [0, 12]
      & ap $ first . (+) <$> [-5, 0, 2, 0, -5, 0, 2, 0]
      & fit ap $ first . (+) <$> [0, 12]
      & fit ap $ first . (+) <$> [0, 12]
      & ap $ first . (+) <$> [0, -3, 5, 2]
      & cycle
