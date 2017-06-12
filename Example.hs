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
main = do
  playIO $ do
    return $ (signal 8000 440 (sineWavetable 8000) & take 8000)
