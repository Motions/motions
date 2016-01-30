{- |
Module      : Bio.Motions.Common
Description : Common utility functions for working with common types.
License     : MIT
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Bio.Motions.Common where

import Bio.Motions.Types

import qualified Data.Vector.Unboxed as U

isLamin :: BinderType -> Bool
isLamin = (== 0) . getBinderType

doesNotBind :: EnergyVector -> Bool
doesNotBind = U.all (== 0) . getEnergyVector

bindsWithLamins :: EnergyVector -> Bool
bindsWithLamins = (/= 0) . (U.! 0) . getEnergyVector

-- |Represents the energy between two objects, e.g. atoms
class HaveEnergyBetween x y where
    -- |Returns the energy between the two objects
    energyBetween :: x -> y -> Energy

instance HaveEnergyBetween EnergyVector BinderType where
    energyBetween (EnergyVector vec) (BinderType ix) = vec U.! ix
    {-# INLINE energyBetween #-}

instance HaveEnergyBetween BinderInfo BeadInfo where
    energyBetween BinderInfo{..} BeadInfo{..} = energyBetween beadEV binderType
    {-# INLINE energyBetween #-}

instance HaveEnergyBetween BeadInfo BinderInfo where
    energyBetween = flip energyBetween
    {-# INLINE energyBetween #-}

instance HaveEnergyBetween Atom Atom where
    energyBetween (Bead beadInfo) (Binder binderInfo) = energyBetween beadInfo binderInfo
    energyBetween (Binder binderInfo) (Bead beadInfo) = energyBetween beadInfo binderInfo
    energyBetween _ _ = 0
    {-# INLINE energyBetween #-}

instance HaveEnergyBetween x y => HaveEnergyBetween (Maybe x) y where
    energyBetween (Just x) y = energyBetween x y
    energyBetween _ _ = 0
    {-# INLINE energyBetween #-}

instance HaveEnergyBetween x y => HaveEnergyBetween x (Maybe y) where
    energyBetween x (Just y) = energyBetween x y
    energyBetween _ _ = 0
    {-# INLINE energyBetween #-}

-- |Represents the objects with position
class HasPosition x where
    -- |A lens returning the spatial position of the object
    position :: Functor f => (Vec3 -> f Vec3) -> x -> f x

instance HasPosition Vec3 where
    position = id
    {-# INLINE position #-}

instance HasPosition BinderInfo where
    position f x = fmap (\p -> x { binderPosition = p }) $ f $ binderPosition x
    {-# INLINE position #-}

instance HasPosition BeadInfo where
    position f x = fmap (\p -> x { beadPosition = p }) $ f $ beadPosition x
    {-# INLINE position #-}

instance HasPosition Atom where
    position f (Bead bead) = Bead <$> position f bead
    position f (Binder binder) = Binder <$> position f binder
    {-# INLINE position #-}
