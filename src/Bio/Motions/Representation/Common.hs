{- |
Module      : Bio.Motions.Representation.Common
Description : Common utility functions for operations on simulation state.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Bio.Motions.Representation.Common where

import Control.Monad
import Control.Lens
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import Linear

import Bio.Motions.Types

type Space' f = M.HashMap Vec3 (Atom' f)
type Space = Space' Identity

-- |A predicate used to determine whether a bead should be frozen
-- or not.
type FreezePredicate = BeadSignature -> Bool

freezeNothing :: FreezePredicate
freezeNothing = const False
{-# INLINE freezeNothing #-}

-- |The legal moves an atom may make
legalMoves :: V.Vector Vec3
legalMoves = V.fromList [v | [x, y, z] <- replicateM 3 [-1, 0, 1],
                             let v = V3 x y z,
                             quadrance v `elem` [1, 2]]
{-# INLINEABLE legalMoves #-}

-- |Checks if a segment connecting the two given points would intersect with a chain.
-- Assumes that these points are neighbours on the 3-dimensional grid, i. e. the quadrance
-- of the distance between these points equals 1 or 2.
intersectsChain :: SpaceLike m space => space (Atom' f) -> Vec3 -> Vec3 -> m Bool
intersectsChain space v1 v2
    | d == 1 = pure False
    | otherwise = do
        vals <- mapM (`mlookup` space) (crossPoss v1 v2)
        pure $ case vals of
                    [Just (Bead b1), Just (Bead b2)] -> chainNeighbours b1 b2
                    _                                -> False
  where
    d = qd v1 v2
    chainNeighbours b1 b2 = b1 ^. beadChain == b2 ^. beadChain
                         && abs (b1 ^. beadIndexOnChain - b2 ^. beadIndexOnChain) == 1
{-# INLINEABLE intersectsChain #-}

-- |Returns the segment crossing the segment connecting the two given points.
-- Assumes that the quadrance between these points equals 2.
-- Returns a two-element list for convenience.
crossPoss :: Vec3 -> Vec3 -> [Vec3]
crossPoss (V3 x1 y1 z1) (V3 x2 y2 z2) | x1 == x2 = [V3 x1 y1 z2, V3 x1 y2 z1]
                                      | y1 == y2 = [V3 x1 y1 z2, V3 x2 y1 z1]
                                      | z1 == z2 = [V3 x1 y2 z1, V3 x2 y1 z1]
{-# INLINEABLE crossPoss #-}

class Monad m => SpaceLike m space where
    mlookup :: Vec3 -> space f -> m (Maybe f)
    minsert :: Vec3 -> f -> space f -> m (space f)
    mdelete :: Vec3 -> space f -> m (space f)
    mmember :: Vec3 -> space f -> m Bool
    mfromList :: [(Vec3, f)] -> m (space f)

instance Monad m => SpaceLike m (M.HashMap Vec3) where
    mlookup vec = pure . M.lookup vec
    {-# INLINE mlookup #-}

    minsert vec val = pure . M.insert vec val
    {-# INLINE minsert #-}

    mdelete vec = pure . M.delete vec
    {-# INLINE mdelete #-}

    mmember vec = pure . M.member vec
    {-# INLINE mmember #-}

    mfromList = pure . M.fromList
    {-# INLINE mfromList #-}


