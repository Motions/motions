{- |
Module      : Bio.Motions.Types
Description : Contains the definitions of common types used in the Motions project.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.Types where

import Linear
import qualified Data.Vector.Unboxed as U

-- |An alias used for representing energy.
type Energy = Int

-- |Represents the binding energy of a bead when connected to a binder of some type
newtype EnergyVector = EnergyVector { getEnergyVector :: U.Vector Int }

-- |Represents a binder type
newtype BinderType = BinderType { getBinderType :: Int }

-- |Represents a binder type
newtype BeadType = BeadType { getBeadType :: Int }

-- |A 3D vector of Ints
type Vec3 = V3 Int

-- |Represents the information about a particular binder
data BinderInfo = BinderInfo
    { binderPosition :: !Vec3   -- ^ The position of the binder
    , binderType :: !BinderType -- ^ The type of the binder
    }

-- |Represents the information about a particular bead
data BeadInfo = BeadInfo
    { beadPosition :: !Vec3 -- ^ The position of the bead
    , beadType :: !BeadType -- ^ The type of the bead
    , beadEV :: !EnergyVector -- ^ The energy vector of the binder
    }

-- |Represents a move of an atom
data Move = Move
    { moveFrom :: !Vec3 -- ^ The previous position of the atom
    , moveDiff :: !Vec3 -- ^ The displacement
    }

-- |Represents an arbitrary atom
data Atom = Bead BeadInfo | Binder BinderInfo

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
    -- |Returns the spatial position of the object
    getPosition :: x -> Vec3

instance HasPosition Vec3 where
    getPosition = id
    {-# INLINE getPosition #-}

instance HasPosition BinderInfo where
    getPosition = binderPosition
    {-# INLINE getPosition #-}

instance HasPosition BeadInfo where
    getPosition = beadPosition
    {-# INLINE getPosition #-}

instance HasPosition Atom where
    getPosition (Bead bead) = getPosition bead
    getPosition (Binder binder) = getPosition binder
    {-# INLINE getPosition #-}
