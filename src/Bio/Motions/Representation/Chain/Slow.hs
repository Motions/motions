{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Bio.Motions.Representation.Chain.Slow where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.Chain.Internal
import Bio.Motions.Utils.Geometry

import Control.Monad.IO.Class
import Control.Lens
import GHC.TypeLits
import GHC.Prim
import Linear

data SlowRepr (r :: Nat) (d :: Nat) = SlowRepr
    { ioRepr :: IOChainRepresentation
    }

instance MonadIO m => ReadRepresentation m (SlowRepr r d) where
    getBinders repr = getBinders $ ioRepr repr
    {-# INLINE getBinders #-}

    getNumberOfChains = getNumberOfChains . ioRepr
    {-# INLINE getNumberOfChains #-}

    getChain repr = getChain $ ioRepr repr
    {-# INLINE getChain #-}

    getAtomAt pos = getAtomAt pos . ioRepr
    {-# INLINE getAtomAt #-}

instance forall r d m. (KnownNat r, KnownNat d, MonadIO m) => Representation m (SlowRepr r d) where
    type ReprRandomTypes m (SlowRepr r d) = ReprRandomTypes m IOChainRepresentation

    loadDump d = fmap SlowRepr . loadDump d
    makeDump = makeDump . ioRepr

    -- TODO: binder intersections?
    generateMove repr@SlowRepr{..} = generateMove' ioRepr (legalMoves r) [] [slowIllegalBeadMove repr]
      where r = fromInteger $ natVal' (proxy# :: Proxy# r)
    {-# INLINE generateMove #-}

    performMove m = fmap SlowRepr . performMove m . ioRepr
    {-# INLINE performMove #-}

-- TODO: binders?
slowIllegalBeadMove :: forall r d m f. (Wrapper m f, MonadIO m, KnownNat d)
    => SlowRepr r d -> Move -> BeadInfo' f -> m Bool
slowIllegalBeadMove SlowRepr{..} Move{..} bead = do
    bead' <- retrieveLocated bead
    pairs <- localNeighbours (bead' & position +~ moveDiff) ioRepr
    pure $ any (uncurry tooFar) pairs || any (uncurry $ intersects $ bead' ^. position) pairs
  where
    tooFar b1 b2 = wrongQd $ qd b1 b2
    intersects b0 b1 b2 = chainIntersectsTriangle maxSegLenSquared (space ioRepr) $ Triangle b0 b1 b2
    wrongQd d = d <= 0 || d > maxSegLenSquared
    maxSegLenSquared = fromInteger $ natVal' (proxy# :: Proxy# d)
{-# INLINE slowIllegalBeadMove #-}
