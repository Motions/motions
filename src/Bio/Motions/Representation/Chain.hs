{- |
Module      : Bio.Motions.Representation.Chain
Description : Contains ineffective chain-based 'Representation's.
License     : Apache
Stability   : experimental
Portability : unportable
 -}

module Bio.Motions.Representation.Chain
    ( PureChainRepresentation
    , IOChainRepresentation
    , ConcurrentChainRepresentation
    ) where

import Bio.Motions.Representation.Chain.Internal
import Bio.Motions.Representation.Chain.Concurrent
