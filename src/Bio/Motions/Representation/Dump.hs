{- |
Module      : Bio.Motions.Representation.Class
Description : Contains the definition of the 'Dump' class.
License     : MIT
Stability   : experimental
Portability : portable
 -}
module Bio.Motions.Representation.Dump where

import Bio.Motions.Types

-- |Represents a dump of the simulation state
data Dump = Dump
    { radius :: Int -- ^ The bounding sphere radius
    , binders :: [BinderInfo] -- ^ A list of binders (in unspecified order)
    , chains :: [[BeadInfo]] -- ^ A list of chains, each represented as a list of beads
    , beadKinds :: [EnergyVector] -- ^ Binding energy vectors for each bead type
    }
