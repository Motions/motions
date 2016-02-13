{- |
Module      : Bio.Motions.Common
Description : Monad transformer used to limit the maximal number of repetitions.
License     : MIT
Stability   : experimental
Portability : unportable
-}

module Bio.Motions.Utils.RepetitionGuard where

import Control.Applicative
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

-- |Transforms monad m into a monad that fails after given number
-- |of counted operations, marked by nextRep
type RepetitionGuardT m = MaybeT (StateT Int m)

-- |Retuns Nothing iff the limit for counted operations was exceeded
runRepetitionGuardT :: (Monad m) => Int -> RepetitionGuardT m a -> m (Maybe a)
runRepetitionGuardT ops_limit = (`evalStateT` ops_limit) . runMaybeT

-- |Marks a counted operation
nextRep :: (Monad m) => RepetitionGuardT m ()
nextRep = do
  count <- get
  guard $ count >= 0
  put $ count - 1

-- |Catches the error of exceeding the oprations limt, so that the
-- |user can hadle it manually
catch :: (Monad m) => RepetitionGuardT m a -> RepetitionGuardT m (Maybe a)
catch = lift . runMaybeT

-- |Inverse of catch, fails when the inner value is Nothing
hide :: (Monad m) => RepetitionGuardT m (Maybe a) -> RepetitionGuardT m a
hide x = x >>= maybe empty return
