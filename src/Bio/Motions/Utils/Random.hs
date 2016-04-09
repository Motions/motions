{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Bio.Motions.Utils.Random where

import Data.Profunctor.Unsafe
import Control.Monad.Primitive
import Control.Monad.Trans
import qualified Control.Monad.Random as CMR
import GHC.Exts
import qualified System.Random.MWC as MWC
import System.IO.Unsafe

class Monad m => MonadRandom m where
    type MonadRandomConstraint m :: * -> Constraint
    getRandom :: MonadRandomConstraint m a => m a
    getRandomR :: MonadRandomConstraint m a => (a, a) -> m a

type family MonadRandomForAll types m where
    MonadRandomForAll '[] m = MonadRandom m
    MonadRandomForAll (t ': ts) m = (MonadRandomConstraint m t, MonadRandom m, MonadRandomForAll ts m)

newtype WithRandom m a = WithRandom { runWithRandom :: m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans WithRandom where
    lift = WithRandom
    {-# INLINE lift #-}

instance CMR.MonadRandom m => MonadRandom (WithRandom m) where
    type MonadRandomConstraint (WithRandom m) = CMR.Random

    getRandom = WithRandom CMR.getRandom
    {-# INLINE getRandom #-}

    getRandomR = WithRandom #. CMR.getRandomR
    {-# INLINE getRandomR #-}

mwcGenIO :: MWC.GenIO
mwcGenIO = unsafePerformIO MWC.createSystemRandom
{-# NOINLINE mwcGenIO #-}

newtype MWCIO a = MWCIO { runMWCIO :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance PrimMonad MWCIO where
    type PrimState MWCIO = PrimState IO
    primitive = MWCIO #. primitive
    {-# INLINE primitive #-}

instance MonadRandom MWCIO where
    type MonadRandomConstraint MWCIO = MWC.Variate

    getRandom = MWC.uniform mwcGenIO
    {-# INLINE getRandom #-}

    getRandomR = flip MWC.uniformR mwcGenIO
    {-# INLINE getRandomR #-}
