module Bio.Motions.RandomShuffle where

import Control.Monad.Random.Class
import Data.List

randomShuffleM :: (MonadRandom m) => [a] -> m [a]
randomShuffleM list = do
  randoms <- getRandoms :: (MonadRandom m => m [Double])
  return $ map snd $ sortBy (\(a,_) (b,_) -> compare a b) $ zip randoms list
