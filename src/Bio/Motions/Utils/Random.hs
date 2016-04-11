{- |
Module      : Bio.Motions.Utils.Random
Description : A unified random number generator interface.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Bio.Motions.Utils.Random
    ( MonadRandom
    , getRandom
    , getRandomR
    , Random
    , Generates
    , WithRandom
    , runWithRandom
    , MWCIO
    , runMWCIO
    ) where

import Data.Profunctor.Unsafe
import Control.Monad.Primitive
import Control.Monad.Trans
import qualified Control.Monad.Random as CMR
import GHC.Exts
import qualified System.Random.MWC as MWC
import System.IO.Unsafe

-- |A 'Monad' which supports generation of (pseud)random numbers.
class Monad m => MonadRandom m where
    -- |The constraint that has to be satisfied by a type for the monad's generaror to generate
    -- a random value of this type.
    type Random m :: * -> Constraint

    -- |Generates a random value.
    getRandom :: Random m a => m a

    -- |Generates a random value in the given range.
    getRandomR :: Random m a => (a, a) -> m a

-- |Constraint ensuring that 'm' is able to generate all the 'types'.
type Generates types m = (MonadRandom m, Randoms types m)

-- |Ensures that the 'Random' 'm' is satisfied for all 'types'.
type family Randoms types m :: Constraint where
    Randoms '[] m = ()
    Randoms (t ': ts) m = (Random m t, Randoms ts m)

-- |Uses 'Control.Monad.Random' as the underlying random number generator.
newtype WithRandom m a = WithRandom { runWithRandom :: m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans WithRandom where
    lift = WithRandom
    {-# INLINE lift #-}

instance CMR.MonadRandom m => MonadRandom (WithRandom m) where
    type Random (WithRandom m) = CMR.Random

    getRandom = WithRandom CMR.getRandom
    {-# INLINE getRandom #-}

    getRandomR = WithRandom #. CMR.getRandomR
    {-# INLINE getRandomR #-}

-- |Unfortunately, GHC is not able to desugar SPECIALISE rules on functions which carry the
-- 'MWC.Gen' using reflection, so as a workaround -- a single global 'MWC.GenIO' is used.
-- May be thread-unsafe.
mwcGenIO :: MWC.GenIO
mwcGenIO = unsafePerformIO MWC.createSystemRandom
{-# NOINLINE mwcGenIO #-}

-- |Uses 'System.Random.MWC' as the underlying random number generator.
newtype MWCIO a = MWCIO { runMWCIO :: IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance PrimMonad MWCIO where
    type PrimState MWCIO = PrimState IO
    primitive = MWCIO #. primitive
    {-# INLINE primitive #-}

instance MonadRandom MWCIO where
    type Random MWCIO = MWC.Variate

    getRandom = MWC.uniform mwcGenIO
    {-# INLINE getRandom #-}

    getRandomR = flip MWC.uniformR mwcGenIO
    {-# INLINE getRandomR #-}
