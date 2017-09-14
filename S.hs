{-# LANGUAGE RankNTypes #-}

import Sound.Synthetic
import Control.Monad (ap, join)
import Data.List
import Data.Bifunctor
import Data.Function ((&))
import Control.Applicative

-- ap transposed
apT :: (Monad m) => m (a -> b) -> m a -> m b
apT f x = fmap (&) x `ap` f

speed :: (Functor f, Functor g) => Float -> f (g Float)  -> f (g Float)
speed n = fmap . fmap $ (/n)

fit :: (Functor g) => ([g Float -> g Float] -> [g Float] -> [g Float]) -> [g Float -> g Float] -> [g Float] -> [g Float]
fit ap' xs = speed (fromIntegral $ length xs) . ap' xs

main :: IO ()
main = play $ tones 8000 (sineWavetable 8000) tones'
  where
   tones' = (fmap . first) midi2cps notes

   notes = [(69, 1)]
     & ((<*>) . fmap (first . (+))) [0, 12]
     & ((<*>) . fmap (first . (+))) [0, 3, 8]
     & ((<*>) . fmap (first . (+))) [0, 12]
     & (fmap . second) (/12)
     & cycle
