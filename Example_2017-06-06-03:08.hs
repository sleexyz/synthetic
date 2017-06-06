import Sound.Synthetic
import Control.Monad (ap, join)
import Data.Function ((&))
import Data.List
import Data.Bifunctor

-- ap transposed
apT :: (Monad m) => m (a -> b) -> m a -> m b
apT f x = fmap (&) x `ap` f

glitch :: Int -> Int -> [a] -> [a]
glitch width rep xs = mconcat (replicate rep l) ++ next
  where
    (l, r) = splitAt width xs
    next = case r of
      [] -> []
      (_:_) -> glitch width rep r

main :: IO ()
main = livecode $ stream

replicateT :: Int -> [a] -> [a]
replicateT n (x:xs) = replicate n x ++ replicateT n xs
replicateT n [] = []

speed :: (Functor f, Functor g) => Double -> f (g Double)  -> f (g Double)
speed n = fmap . fmap $ (/n)

fit :: (Functor g) => ([g Double -> g Double] -> [g Double] -> [g Double]) -> [g Double -> g Double] -> [g Double] -> [g Double]
fit ap' xs = speed (fromIntegral $ length xs) . ap' xs

stream :: [Double]
stream = tones 8000 table tonePairs
  where
    table = []
      & mappend (sawWavetable 8000)
      & fmap (tanh)

    tonePairs = (fmap . first) midi2cps notePairs

    notePairs = (cycle notes `zip` cycle durations)
      & fit apT (first . flip (+) <$> [0, -12, -19, -24, -28])

    notes = [0]
      & apT (fmap (+) [0, 12])
      & apT (fmap (+) [0, -2])
      & apT (fmap (+) [0, -2])
      & apT (fmap (+) [0, 5])
      & apT (fmap (+) [0, 12])
      & ap [(+72)]

    durations = [1]
      & ap [(/2)]
