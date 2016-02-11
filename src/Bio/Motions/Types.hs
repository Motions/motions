{- |
Module      : Bio.Motions.Types
Description : Contains the definitions of common types used in the Motions project.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Bio.Motions.Types where

import Linear
import qualified Data.Vector.Unboxed as U
import GHC.Exts
import Control.Lens.TH

-- |An alias used for representing energy.
type Energy = Int

-- |Represents the binding energy of a bead when connected to a binder of some type.
-- EnergyVectors must have at least one element. The first element always represents
-- the energy of binding with lamins.
newtype EnergyVector = EnergyVector { getEnergyVector :: U.Vector Int }
    deriving (Eq, Show, Ord)

instance IsList EnergyVector where
    type Item EnergyVector = Int
    fromList = EnergyVector . fromList
    toList = toList . getEnergyVector

-- |Represents a binder type
newtype BinderType = BinderType { getBinderType :: Int }
    deriving (Eq, Show, Ord)

-- |Represents a chain identifier
type ChainId = Int

-- |A 3D vector of Ints
type Vec3 = V3 Int

-- |Represents the immutable information about a particular bead
data BeadSignature = BeadSignature
    { _beadEV :: !EnergyVector -- ^ The energy vector of the bead
    , _beadAtomIndex :: !Int -- ^ The global index of this bead
    , _beadChain :: !Int -- ^ The index of the chain this bead belongs to
    , _beadIndexOnChain :: !Int -- ^ The index on the chain
    }
    deriving (Eq, Show)
makeClassy ''BeadSignature

-- |Represents the immutable information about a particular binder
newtype BinderSignature = BinderSignature
    { _binderType :: BinderType -- ^ The type of the binder
    }
    deriving (Eq, Show)
makeClassy ''BinderSignature

-- |Adds position information to an arbitrary object.
data Located a = Located
    { _location :: !Vec3
    , _located  :: a
    }
    deriving (Eq, Show, Functor)
makeLenses ''Located

type BeadInfo = Located BeadSignature
type BinderInfo = Located BinderSignature

-- |Represents a move of an atom
data Move = Move
    { moveFrom :: !Vec3 -- ^ The previous location of the atom
    , moveDiff :: !Vec3 -- ^ The displacement
    }
    deriving (Eq, Show)

pattern MoveFromTo from to <- Move from ((+from) -> to) where
    MoveFromTo from to = Move from (to - from)

-- |Represents an arbitrary atom
data AtomSignature = BeadSig { getBeadSignature :: BeadSignature }
                   | BinderSig { getBinderSignature :: BinderSignature }
    deriving (Eq, Show)

type Atom = Located AtomSignature

pattern Bead b <- Located _ (BeadSig b)
pattern Binder b <- Located _ (BinderSig b)

pattern BinderInfo pos binderType = Located pos (BinderSignature binderType)
pattern BeadInfo pos ev atomIx chainIx ixOnChain = Located pos (BeadSignature ev atomIx chainIx ixOnChain)

-- |Represents an additional addition or removal of a binder
-- due to a 'Move'.
data BinderChange = AddBinder BinderInfo -- ^ Addition of a binder
                  | RemoveBinder BinderInfo -- ^ Removal of a binder

instance HasBeadSignature BeadInfo where
    beadSignature = located

instance HasBinderSignature BinderInfo where
    binderSignature = located
