{- |
Module      : Bio.Motions.Types
Description : Contains the definitions of common types used in the Motions project.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Bio.Motions.Types where

import Linear
import qualified Data.Vector.Unboxed as U

-- |An alias used for representing energy.
type Energy = Int

-- |Represents the binding energy of a bead when connected to a binder of some type.
-- EnergyVectors must have at least one element. The first element always represents
-- the energy of binding with lamins.
newtype EnergyVector = EnergyVector { getEnergyVector :: U.Vector Int }
    deriving (Eq, Show, Ord)

-- |Represents a binder type
newtype BinderType = BinderType { getBinderType :: Int }
    deriving (Eq, Show, Ord)

-- |Represents a bead type
newtype BeadType = BeadType { getBeadType :: Int }
    deriving (Eq, Show)

-- |Represents a chain identifier
type ChainId = Int

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
    , beadEV :: !EnergyVector -- ^ The energy vector of the bead
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
