{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module Bio.Motions.Utils.Random where

import Data.Profunctor.Unsafe
import Data.Proxy
import Data.Reflection
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

withRandom :: WithRandom m a -> m a
withRandom = runWithRandom
{-# INLINE withRandom #-}

{- niestety używanie tego jest too complicated dla GHC :( -}
newtype WithMWC s m a = WithMWC { runWithMWC :: m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (WithMWC s) where
    lift = WithMWC
    {-# INLINE lift #-}

instance PrimMonad m => PrimMonad (WithMWC s m) where
    type PrimState (WithMWC s m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}

instance (Reifies s (MWC.Gen (PrimState m)), PrimMonad m) => MonadRandom (WithMWC s m) where
    type MonadRandomConstraint (WithMWC s m) = MWC.Variate

    getRandom = MWC.uniform (reflect (Proxy :: Proxy s))
    {-# INLINE getRandom #-}

    getRandomR = flip MWC.uniformR (reflect (Proxy :: Proxy s))
    {-# INLINE getRandomR #-}

withMWC :: forall a. (forall s. MonadRandom (WithMWC s IO) => WithMWC s IO a) -> IO a
withMWC f = MWC.withSystemRandom go
  where
    go :: MWC.GenIO -> IO a
    go gen = reify gen (\(_ :: Proxy s) -> runWithMWC (f :: WithMWC s IO a))
    {-# INLINE go #-}
{-# INLINE withMWC #-}

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

withMWCIO :: MWCIO a -> IO a
withMWCIO = runMWCIO
{-# INLINE withMWCIO #-}
