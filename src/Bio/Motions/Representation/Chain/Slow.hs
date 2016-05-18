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
import Data.List
import Data.Ix
import Data.Maybe
import qualified Data.HashMap.Strict as M
import Linear
import Math.NumberTheory.Powers.Squares

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
      where
        r :: Int
        r = fromInteger $ natVal' (proxy# :: Proxy# r)
    {-# INLINE generateMove #-}

    performMove m = fmap SlowRepr . performMove m . ioRepr
    {-# INLINE performMove #-}

-- TODO: binders?
slowIllegalBeadMove :: forall r d m f. (Wrapper m f, MonadIO m, KnownNat d)
    => SlowRepr r d -> Move -> BeadInfo' f -> m Bool
slowIllegalBeadMove repr@SlowRepr{..} Move{..} bead = do
    bead' <- retrieveLocated bead
    pairs <- localNeighbours (bead' & position +~ moveDiff) ioRepr
    pure $ any (uncurry tooFar) pairs || any (uncurry $ intersects $ bead' ^. position) pairs
  where
    tooFar b1 b2 = wrongQd $ qd b1 b2
    intersects b0 b1 b2 = chainIntersectsTriangle repr $ Triangle b0 b1 b2
    wrongQd d = d <= 0 || d > maxSegLenSquared
    maxSegLenSquared = fromInteger $ natVal' (proxy# :: Proxy# d)

chainIntersectsTriangle :: forall r d. KnownNat d => SlowRepr r d -> Triangle -> Bool
chainIntersectsTriangle repr t = any (intersectsTriangle t) segs
  where
    segs = connectedBeads $ beadPositionsInCube repr cube
    cube = extendedCube maxSegLen $ boundingCube [v1 t, v2 t, v3 t]
    maxSegLen = integerSquareRoot . fromInteger $ natVal' (proxy# :: Proxy# d)

beadPositionsInCube :: SlowRepr r d -> Cube -> [BeadInfo]
beadPositionsInCube SlowRepr{..} Cube{..} = mapMaybe getBeadInfo allPositions -- TODO: kd-tree?
  where
    getBeadInfo pos = do
        Bead b <- M.lookup pos $ space ioRepr
        pure $ Located pos b
    allPositions = range (minCorner, maxCorner)

-- TODO: złożoność?
connectedBeads :: [BeadInfo] -> [Segment]
connectedBeads beads = [Segment (b1 ^. position) (b2 ^. position)
                            | (b1, b2) <- distinctPairs beads
                            , chainNeighbours (b1 ^. located) (b2 ^. located)]

distinctPairs :: [a] -> [(a, a)]
distinctPairs l = [(a, b) | a : tl <- init (tails l), b <- tl]
