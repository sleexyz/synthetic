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

fit :: (Functor f) => ([a] -> [f Double] -> [f Double]) -> [a] -> [f Double] -> [f Double]
fit mod xs = speed (fromIntegral $ length xs) . mod xs

noteMod :: [Double] -> [(Double, Double)] -> [(Double, Double)]
noteMod = ap . fmap (first . (+))

-- moving average lowpass
lowPass :: Int -> [Double] -> [Double]
lowPass n xs =  xs
  & zipWith (+) (drop n xs)
  & fmap (/fromIntegral (n + 1))

stream :: [Double]
stream = tones 8000 table tonePairs
  & lowPass 4
  where
    table = []
      & mappend (sineWavetable 8000)
      & fmap (tanh)

    tonePairs = (fmap . first) midi2cps notePairs

    notePairs = [(60, 1/2)]
      & noteMod [0, 4, 16, 28]
      & noteMod [0, -7]
      & noteMod [0, -7]
      & fit noteMod [0, 5, 5, -7]
      & cycle
