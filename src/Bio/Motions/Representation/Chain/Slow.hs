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
import Bio.Motions.Utils.Random

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Lens
import GHC.TypeLits
import GHC.Prim
import Data.List
import Data.Ix
import Data.Maybe
import Data.MonoTraversable
import qualified Data.HashMap.Strict as M
import qualified Data.Sequences as DS
import Linear

data SlowRepr (r :: Nat) (d :: Nat) = SlowRepr
    { ioRepr :: IOChainRepresentation
    }

instance MonadIO m => ReadRepresentation m (SlowRepr r d) where
    getBinders r = getBinders $ ioRepr r
    {-# INLINE getBinders #-}

    getNumberOfChains = getNumberOfChains . ioRepr
    {-# INLINE getNumberOfChains #-}

    getChain = undefined
    getAtomAt = undefined

instance forall r d m. (KnownNat r, KnownNat d, MonadIO m) => Representation m (SlowRepr r d) where
    type ReprRandomTypes m (SlowRepr r d) = ReprRandomTypes m IOChainRepresentation

    loadDump = undefined
    makeDump = undefined

    generateMove repr@SlowRepr{..} = do
        let ChainRepresentation{..} = ioRepr
        moveBinder <- getRandom
        if moveBinder then
            pick moveableBinders binders [] -- TODO: binder intersections
        else
            pick moveableBeads beads [illegalBeadMove' repr]
      where
        pick :: _ => ixs -> s -> t (Move -> Element s -> m Bool) -> m (Maybe Move)
        pick ixs xs constraints = do
            idx <- getRandomElement ixs
            let x = DS.unsafeIndex xs idx
            d <- getRandomElement $ legalMoves' r
            pos <- (^. position) <$> retrieveLocated x
            let pos' = pos + d
            runMaybeT $ do
                guard . not $ M.member pos' (space ioRepr)
                let m = Move pos d
                forM_ constraints $ \c -> lift (c m x) >>= guard . not
                pure m

        r :: Int
        r = fromInteger $ natVal' (proxy# :: Proxy# r)

    performMove = undefined

-- TODO: binders?
illegalBeadMove' :: forall r d m f. (Wrapper m f, MonadIO m, KnownNat d)
    => SlowRepr r d -> Move -> BeadInfo' f -> m Bool
illegalBeadMove' repr@SlowRepr{..} Move{..} bead = do
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
    maxSegLen = fromInteger $ natVal' (proxy# :: Proxy# d) -- FIXME: sqrt

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
