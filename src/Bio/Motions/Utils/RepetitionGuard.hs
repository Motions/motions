module Bio.Motions.Utils.RepetitionGuard where

import Control.Applicative
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

type RepetitionGuardT m = MaybeT (StateT Int m)

runRepetitionGuard :: (Monad m) => Int -> RepetitionGuardT m a -> m (Maybe a)
runRepetitionGuard lim = fmap fst . (`runStateT` lim) . runMaybeT

nextRep :: (Monad m) => RepetitionGuardT m ()
nextRep = do
  count <- get
  put $ count - 1
  guard $ count >= 0

catch :: (Monad m) => RepetitionGuardT m a -> RepetitionGuardT m (Maybe a)
catch = lift . runMaybeT

hide :: (Monad m) => RepetitionGuardT m (Maybe a) -> RepetitionGuardT m a
hide x = x >>= maybe empty return
