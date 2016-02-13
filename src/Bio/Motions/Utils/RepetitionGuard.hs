{- |
Module      : Bio.Motions.Utils.RepetitionGuard
Description : Monad transformer used to limit the number of repetitions.
License     : MIT
Stability   : experimental
Portability : unportable
-}

module Bio.Motions.Utils.RepetitionGuard where

import Control.Applicative
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

-- |Transforms 'Monad' 'm' into a monad that limits the number of 'nextReps's
type RepetitionGuardT m = MaybeT (StateT Int m)

-- |Returns Nothing iff the limit for counted operations was exceeded
runRepetitionGuardT :: (Monad m) => Int -> RepetitionGuardT m a -> m (Maybe a)
runRepetitionGuardT opsLimit = (`evalStateT` opsLimit) . runMaybeT

-- |Marks a counted operation
nextRep :: (Monad m) => RepetitionGuardT m ()
nextRep = do
  count <- get
  guard $ count >= 0
  put $ count - 1

-- |Catches the error of exceeding the operations' limit, so that the
-- |user can handle it manually
catch :: (Monad m) => RepetitionGuardT m a -> RepetitionGuardT m (Maybe a)
catch = lift . runMaybeT

-- |Inverse of catch, fails when the inner value is Nothing
hide :: (Monad m) => RepetitionGuardT m (Maybe a) -> RepetitionGuardT m a
hide x = x >>= maybe empty return
