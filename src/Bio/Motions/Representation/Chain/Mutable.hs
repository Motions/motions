{- |
Module      : Bio.Motions.Representation.Chain.Mutable
Description : Contains a mutable chain-based 'Representation'.
License     : MIT
Stability   : experimental
Portability : unportable
 -}

module Bio.Motions.Representation.Chain.Mutable
    ( IOChainRepresentation
    , STChainRepresentation
    ) where

import Bio.Motions.Representation.Chain.Internal (MutableChainRepresentation)
import Control.Monad.ST

type IOChainRepresentation = MutableChainRepresentation RealWorld
type STChainRepresentation s = MutableChainRepresentation s
