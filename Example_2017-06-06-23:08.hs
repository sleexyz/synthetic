import Sound.Synthetic
import Control.Monad (ap, join)
import Data.Function ((&))
import Data.List
import Data.Bifunctor

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
      & zipWith (*) (reverse $ sawWavetable 8000)
      & fmap (tanh)

    tonePairs = (fmap . first) midi2cps notePairs

    notePairs = [(60, 1/2)]
      & ap (first . (+) <$> [1, 4, 9])
      & fit ap (first . (+) <$> [0, 12]) 
      & fit ap (first . (+) <$> [0, -7])
      & ap (first . (+) <$> [0, -3, 2])
      & cycle
