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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Bio.Motions.Types where

import Linear
import qualified Data.Vector.Unboxed as U

-- |An alias used for representing energy.
type Energy = Int

-- |Represents the binding energy of a bead when connected to a binder of some type
newtype EnergyVector = EnergyVector { getEnergyVector :: U.Vector Int }
    deriving (Eq, Show)

-- |Represents a binder type
newtype BinderType = BinderType { getBinderType :: Int }
    deriving (Eq, Show)

-- |Represents a binder type
newtype BeadType = BeadType { getBeadType :: Int }
    deriving (Eq, Show)

-- |A 3D vector of Ints
type Vec3 = V3 Int

-- |Represents the information about a particular binder
data BinderInfo = BinderInfo
    { binderPosition :: !Vec3   -- ^ The position of the binder
    , binderType :: !BinderType -- ^ The type of the binder
    }
    deriving (Eq, Show)

-- |Represents the information about a particular bead
data BeadInfo = BeadInfo
    { beadPosition :: !Vec3 -- ^ The position of the bead
    , beadType :: !BeadType -- ^ The type of the bead
    , beadEV :: !EnergyVector -- ^ The energy vector of the binder
    , beadAtomIndex :: !Int -- ^ The global index of this bead
    , beadChain :: !Int -- ^ The index of the chain this bead belongs to
    , beadIndexOnChain :: !Int -- ^ The index on the chain
    }
    deriving (Eq, Show)

-- |Represents a move of an atom
data Move = Move
    { moveFrom :: !Vec3 -- ^ The previous position of the atom
    , moveDiff :: !Vec3 -- ^ The displacement
    }
    deriving (Eq, Show)

pattern MoveFromTo from to <- Move from ((+from) -> to) where
    MoveFromTo from to = Move from (to - from)

-- |Represents an arbitrary atom
data Atom = Bead { getBeadInfo :: BeadInfo }
          | Binder { getBinderInfo :: BinderInfo }
    deriving (Eq, Show)

-- |Represents an additional addition or removal of a binder
-- due to a 'Move'.
data BinderChange = AddBinder BinderInfo -- ^ Addition of a binder
                  | RemoveBinder BinderInfo -- ^ Removal of a binder

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
