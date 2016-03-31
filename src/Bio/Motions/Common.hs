{- |
Module      : Bio.Motions.Common
Description : Common utility functions for working with common types.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Bio.Motions.Common where

import Bio.Motions.Types
import Control.Lens
import Crypto.Random.Types
import Crypto.Number.Generate
import qualified Data.Vector.Unboxed as U
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad.State as LS
import Control.Monad.State.Strict as SS
import System.Random.Shuffle(shuffle)

laminType :: BinderType
laminType = BinderType 0

doesNotBind :: EnergyVector -> Bool
doesNotBind = U.all (== 0) . getEnergyVector

bindsWithLamins :: EnergyVector -> Bool
bindsWithLamins = (/= 0) . (U.! getBinderType laminType) . getEnergyVector

instance MonadRandom m => MonadRandom (MaybeT m) where
    getRandomBytes = lift . getRandomBytes

instance MonadRandom m => MonadRandom (LS.StateT s m) where
    getRandomBytes = lift . getRandomBytes

instance MonadRandom m => MonadRandom (SS.StateT s m) where
    getRandomBytes = lift . getRandomBytes

shuffleM :: (MonadRandom m) => [a] -> m [a]
shuffleM elements
    | null elements = return []
    | otherwise     = fmap (shuffle elements . fmap fromIntegral) (rseqM $ fromIntegral (length elements - 1))
        where
            rseqM :: (MonadRandom m) => Integer -> m [Integer]
            rseqM 0 = return []
            rseqM i = liftM2 (:) (generateBetween 0 i) (rseqM (i - 1))

-- |Represents the energy between two objects, e.g. atoms
class HaveEnergyBetween x y where
    -- |Returns the energy between the two objects
    energyBetween :: x -> y -> Energy

instance HaveEnergyBetween EnergyVector BinderType where
    energyBetween (EnergyVector vec) (BinderType ix) = vec U.! ix
    {-# INLINE energyBetween #-}

instance HaveEnergyBetween BeadSignature BinderSignature where
    energyBetween x y = energyBetween (x ^. beadEV) (y ^. binderType)
    {-# INLINE energyBetween #-}

instance HaveEnergyBetween BinderSignature BeadSignature where
    energyBetween = flip energyBetween
    {-# INLINE energyBetween #-}

instance HaveEnergyBetween AtomSignature AtomSignature where
    energyBetween (BeadSig beadInfo) (BinderSig binderInfo) = energyBetween beadInfo binderInfo
    energyBetween (BinderSig binderInfo) (BeadSig beadInfo) = energyBetween beadInfo binderInfo
    energyBetween _ _ = 0
    {-# INLINE energyBetween #-}

instance {-# INCOHERENT #-} HaveEnergyBetween x y => HaveEnergyBetween (Located x) y where
    energyBetween x = energyBetween (x ^. located)
    {-# INLINE energyBetween #-}

instance {-# INCOHERENT #-} HaveEnergyBetween x y => HaveEnergyBetween x (Located y) where
    energyBetween x y = energyBetween x (y ^. located)
    {-# INLINE energyBetween #-}

instance {-# INCOHERENT #-} HaveEnergyBetween x y => HaveEnergyBetween (Maybe x) y where
    energyBetween (Just x) y = energyBetween x y
    energyBetween _ _ = 0
    {-# INLINE energyBetween #-}

instance {-# INCOHERENT #-} HaveEnergyBetween x y => HaveEnergyBetween x (Maybe y) where
    energyBetween x (Just y) = energyBetween x y
    energyBetween _ _ = 0
    {-# INLINE energyBetween #-}

-- |A convenient unwrapper of 'wrappedPosition'.
position :: Lens' (Located a) Vec3
position = wrappedPosition . _Wrapping Identity
{-# INLINE position #-}

class AsAtom' f a where
    asAtom' :: a -> Atom' f

instance AsAtom' f (Atom' f) where
    asAtom' = id
    {-# INLINE asAtom' #-}

instance AsAtom' f (BinderInfo' f) where
    asAtom' = fmap BinderSig
    {-# INLINE asAtom' #-}

instance AsAtom' f (BeadInfo' f) where
    asAtom' = fmap BeadSig
    {-# INLINE asAtom' #-}

-- |A type-constrained version of 'asAtom''.
asAtom :: AsAtom' Identity a => a -> Atom
asAtom = asAtom'
{-# INLINE asAtom #-}

type AsAtom a = AsAtom' Identity a
