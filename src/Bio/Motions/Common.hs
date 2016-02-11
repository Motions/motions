{- |
Module      : Bio.Motions.Common
Description : Common utility functions for working with common types.
License     : MIT
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Bio.Motions.Common where

import Bio.Motions.Types
import Control.Lens
import qualified Data.Vector.Unboxed as U

laminType :: BinderType
laminType = BinderType 0

doesNotBind :: EnergyVector -> Bool
doesNotBind = U.all (== 0) . getEnergyVector

bindsWithLamins :: EnergyVector -> Bool
bindsWithLamins = (/= 0) . (U.! getBinderType laminType) . getEnergyVector

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

instance HaveEnergyBetween x y => HaveEnergyBetween (Maybe x) y where
    energyBetween (Just x) y = energyBetween x y
    energyBetween _ _ = 0
    {-# INLINE energyBetween #-}

instance HaveEnergyBetween x y => HaveEnergyBetween x (Maybe y) where
    energyBetween x (Just y) = energyBetween x y
    energyBetween _ _ = 0
    {-# INLINE energyBetween #-}

-- |Represents objects having spatial position
class HasPosition x where
    position :: Lens' x Vec3

instance HasPosition Vec3 where
    position = id
    {-# INLINE position #-}

instance HasPosition (Located x) where
    position = location
    {-# INLINE position #-}

class AsAtom a where
    asAtom :: a -> Atom

instance AsAtom Atom where
    asAtom = id
    {-# INLINE asAtom #-}

instance AsAtom BinderInfo where
    asAtom = fmap BinderSig
    {-# INLINE asAtom #-}

instance AsAtom BeadInfo where
    asAtom = fmap BeadSig
    {-# INLINE asAtom #-}
