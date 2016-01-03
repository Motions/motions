{- |
Module      : Bio.Motions.Representation.Class
Description : Contains the definition of the 'Representation' class.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Bio.Motions.Representation.Class where

import Bio.Motions.Types
import qualified Bio.Motions.Representation.Dump as Dump

import Control.Applicative
import Control.Monad.Random
import Data.MonoTraversable


-- |Stores the simulation state and performs basic operations on it
--
-- 'm' denotes a 'Monad' (or 'Applicative') in which the
-- simulation takes place
class ReadRepresentation m repr => Representation m repr where
    -- |Loads the state from a 'Dump.Dump'
    loadDump :: Dump.Dump -> m repr

    -- |Saves the current state in a 'Dump.Dump'
    makeDump :: repr -> m Dump.Dump

    -- |Generates a random valid 'Move' or 'empty'.
    generateMove :: (MonadRandom m, Alternative m) => repr -> m Move

    -- |Applies a 'Move' to the state
    performMove :: Move -> repr -> m repr

-- |A read-only interface to a 'Representation' 
class ReadRepresentation m repr where
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
