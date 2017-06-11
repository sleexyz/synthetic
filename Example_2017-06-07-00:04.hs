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

speed :: (Functor f, Functor g) => Double -> f (g Double)  -> f (g Double)
speed n = fmap . fmap $ (/n)

fit :: (Functor g) => ([g Double -> g Double] -> [g Double] -> [g Double]) -> [g Double -> g Double] -> [g Double] -> [g Double]
fit ap' xs = speed (fromIntegral $ length xs) . ap' xs

stream :: [Double]
stream = tones 8000 table tonePairs
  where
    table = []
      & mappend (sineWavetable 8000)
      & fmap (tanh)

    tonePairs = (fmap . first) midi2cps notePairs

    notePairs = [(72, 1/2)]
      & ap $ first . (+) <$> [0, 3, 8]
      & fit ap $ first . (+) <$> [0, 12]
      & fit ap $ first . (+) <$> [0, -7]
      & ap $ first . (+) <$> [0, -7]
      & ap $ first . (+) <$> [0, -3, 2]
      & cycle
