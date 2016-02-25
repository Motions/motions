{- |
Module      : Bio.Motions.Utils.RepetitionGuard
Description : Monad transformer used to limit the number of repetitions.
License     : Apache
Stability   : experimental
Portability : unportable
-}

module Bio.Motions.Utils.RepetitionGuard where

import Control.Monad.State
import Control.Monad.Trans.Maybe

-- |Transforms a 'Monad' 'm' into a monad that limits the number of 'nextRep's
type RepetitionGuardT m = MaybeT (StateT Int m)

-- |Returns 'Nothing' iff the limit for counted operations was exceeded
runRepetitionGuardT :: (Monad m) => Int -> RepetitionGuardT m a -> m (Maybe a)
runRepetitionGuardT opsLimit = (`evalStateT` opsLimit) . runMaybeT

-- |Marks a counted operation
nextRep :: (Monad m) => RepetitionGuardT m ()
nextRep = do
    count <- get
    guard $ count >= 0
    put $ count - 1
