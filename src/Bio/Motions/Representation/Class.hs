{- |
Module      : Bio.Motions.Representation.Class
Description : Contains the definition of the 'Representation' class.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Bio.Motions.Representation.Class where

import Bio.Motions.Types
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Random

import Data.MonoTraversable
import GHC.Exts


-- |Stores the simulation state and performs basic operations on it
--
-- 'm' denotes a 'Monad' (or 'Applicative') in which the
-- simulation takes place
class (ReprExposedConstraint m repr, ReadRepresentation m repr) => Representation m repr where

    -- |An additional constraint which is retrievable given the dictionary for this Representation.
    -- Note that this cannot be achieved by specifying the constraint only in the instance context,
    -- but it is necessary to specify them in the class context.
    --
    -- Used to specialise the 'Bio.Motions.Chain.Slow.SlowChainRepresentation' instances in the
    -- "Main" module.
    type ReprExposedConstraint m repr :: Constraint

    -- |Loads the state from a 'Dump'
    loadDump :: Dump -> FreezePredicate -> m repr

    -- |Applies a 'Move' to the state
    performMove :: Move -> repr -> m repr

-- |A read-only interface to a 'Representation'
class CallbackRepresentation m repr => ReadRepresentation  m repr where
    -- |Types that the representation wants to be able to sample randomly in 'generateMove'.
    type ReprRandomTypes m repr :: [*]

    -- |Saves the current state in a 'Dump'
    makeDump :: repr -> m Dump

    -- |Generates a random valid 'Move' or 'Nothing'.
    generateMove :: Generates (ReprRandomTypes m repr) m => repr -> m (Maybe Move)


class CallbackRepresentation m repr where
    -- |Retrieves an arbitrary information about the represented binders
    -- Note: the implementation is allowed to return only a subset of binders,
    -- provided that every binder bound to some bead is represented.
    getBinders ::
        repr
        -- ^ The representation
        -> (forall c. (MonoTraversable c, Element c ~ BinderInfo) => c -> m res)
        -- ^A function that will be given a 'MonoTraversable' containing the binders' data.
        -- Note: This 'MonoTraversable' must be neither saved nor returned by the function.
        -> m res
        -- ^ The return value of the above function

    -- |Retrieves the number of chains
    getNumberOfChains :: repr -> m Int

    -- |Retrieves an arbitrary information about a chain
    getChain ::
        repr
        -- ^ The representation
        -> Int
        -- ^The chain number. Chains are numbered 0..(n - 1) where n is the number of chains
        -> (forall c. (MonoTraversable c, Element c ~ BeadInfo) => c -> m res)
        -- ^The function which will be given a 'MonoTraversable' containing the beads' data
        -- on the particular chain.
        -- Note: This 'MonoTraversable' must be neither saved nor returned by the function.
        -> m res
        -- ^ The return value of the above function

    -- |Retrieves the atom at the specified spatial position
    getAtomAt :: Vec3 -> repr -> m (Maybe Atom)
