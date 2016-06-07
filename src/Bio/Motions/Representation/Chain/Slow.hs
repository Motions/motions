{- |
Module      : Bio.Motions.Representation.Chain.Slow
Description : Contains an IOChain based representation that allows
              arbitrary move radius and chain segment length. May be slower.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
module Bio.Motions.Representation.Chain.Slow where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.Chain.Internal

import Control.Monad.IO.Class
import Control.Lens
import qualified Data.Vector as V
import Data.Maybe
import GHC.TypeLits
import GHC.Prim
import Linear

data SlowChainRepresentation (r :: Nat) (d :: Nat) = SlowChainRepresentation
    { ioRepr :: !IOChainRepresentation
    , legalMoves' :: !(V.Vector Vec3)
    , maxSegLenSquared :: !Int
    }

instance MonadIO m => ReadRepresentation m (SlowChainRepresentation r d) where
    getBinders repr = getBinders $ ioRepr repr
    {-# INLINE getBinders #-}

    getNumberOfChains = getNumberOfChains . ioRepr
    {-# INLINE getNumberOfChains #-}

    getChain repr = getChain $ ioRepr repr
    {-# INLINE getChain #-}

    getAtomAt pos = getAtomAt pos . ioRepr
    {-# INLINE getAtomAt #-}

instance (KnownNat r, KnownNat d, MonadIO m) => Representation m (SlowChainRepresentation r d) where
    type ReprRandomTypes m (SlowChainRepresentation r d) = ReprRandomTypes m IOChainRepresentation
    type ReprExposedConstraint m (SlowChainRepresentation r d) = (KnownNat r, KnownNat d)

    loadDump d p = loadDump d p >>= \ioRepr -> pure SlowChainRepresentation{..}
      where
        legalMoves' = legalMoves maxMoveRadSquared
        maxMoveRadSquared = fromInteger $ natVal' (proxy# :: Proxy# r)
        maxSegLenSquared = fromInteger $ natVal' (proxy# :: Proxy# d)

    makeDump = makeDump . ioRepr

    -- TODO: binder intersections?
    generateMove repr@SlowChainRepresentation{..} =
        generateMove' ioRepr legalMoves' Nothing (Just $ slowIllegalBeadMove repr)
    {-# INLINE generateMove #-}

    performMove m repr = performMove m (ioRepr repr) >>= \ioRepr' -> pure repr { ioRepr = ioRepr' }
    {-# INLINE performMove #-}

-- TODO: binders?
slowIllegalBeadMove :: forall r d m f. (Wrapper m f, MonadIO m, KnownNat d)
    => SlowChainRepresentation r d -> Move -> BeadInfo' f -> m Bool
slowIllegalBeadMove SlowChainRepresentation{..} move@Move{..} bead = do
    bead' <- retrieveLocated bead
    neighbours <- mapM (unwrap . (^. wrappedPosition)) . catMaybes $
        [ chain V.!? (idx - 1)
        , chain V.!? (idx + 1)
        ]
    pairs <- localNeighbours (bead' & position +~ moveDiff) ioRepr
    pure $ any (wrongQd . uncurry qd) pairs
        || chainIntersectsMove maxSegLenSquared (space ioRepr) move neighbours
  where
    idx = bead ^. beadIndexOnChain
    chain = getChain' ioRepr $ bead ^. beadChain
    wrongQd d = d <= 0 || d > maxSegLenSquared
{-# INLINE slowIllegalBeadMove #-}
