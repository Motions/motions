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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
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

newtype SlowRepr (r :: Nat) (d :: Nat) = SlowRepr
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
slowIllegalBeadMove SlowRepr{..} move@Move{..} bead = do
    bead' <- retrieveLocated bead
    neighbours <- sequence $ catMaybes
        [ fmap retrieveLocated $ chain V.!? (idx - 1)
        , fmap retrieveLocated $ chain V.!? (idx + 1)
        ]
    let neighbourPositions = view position <$> neighbours
    pairs <- localNeighbours (bead' & position +~ moveDiff) ioRepr
    pure $ any (wrongQd . uncurry qd) pairs
        || chainIntersectsMove maxSegLenSquared (space ioRepr) move neighbourPositions
  where
    idx = bead ^. beadIndexOnChain
    chain = getChain' ioRepr $ bead ^. beadChain
    wrongQd d = d <= 0 || d > maxSegLenSquared
    maxSegLenSquared = fromInteger $ natVal' (proxy# :: Proxy# d)
{-# INLINE slowIllegalBeadMove #-}