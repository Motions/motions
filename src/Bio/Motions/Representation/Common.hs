{- |
Module      : Bio.Motions.Representation.Common
Description : Common utility functions for operations on simulation state.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Bio.Motions.Representation.Common where

import Control.Monad
import Control.Lens
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import Data.Ix
import Data.Maybe
import Math.NumberTheory.Powers.Squares
import Linear

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Utils.Geometry
import Bio.Motions.Utils.Common

type Space' f = M.HashMap Vec3 (Atom' f)
type Space = Space' Identity

-- |A predicate used to determine whether a bead should be frozen
-- or not.
type FreezePredicate = BeadSignature -> Bool

freezeNothing :: FreezePredicate
freezeNothing = const False

-- |The legal moves an atom may make.
legalMoves ::
     Int
     -- ^Square of the maximum move radius
  -> V.Vector Vec3
     -- ^The resulting vector of moves
legalMoves r = V.fromList [v | [x, y, z] <- replicateM 3 [-rt..rt],
                               let v = V3 x y z,
                               quadrance v `elem` [1..r]]
  where rt = integerSquareRoot r

-- |Tests whether the given triangle is intersected by a chain in 3D space.
chainIntersectsTriangle ::
     Int
  -- ^Square of the maximum chain segment length.
  -> Space' f
  -- ^The space.
  -> Triangle
  -- ^The triangle.
  -> Bool
  -- ^The result.
chainIntersectsTriangle maxSegLenSquared space triangle = any (intersectsTriangle triangle) segs
  where
    segs = connectedBeads $ beadsInCube space cube
    cube = extendedCube rt . boundingCube . map ($ triangle) $ [v1, v2, v3]
    rt = integerSquareRoot maxSegLenSquared

-- |Tests whether a segment connecting the two given points is intersected by a chain in 3D space.
intersectsChain ::
     Int
  -- ^Square of the maximum chain segment length.
  -> Space' f
  -- ^The space.
  -> Vec3
  -- ^First point.
  -> Vec3
  -- ^Second point.
  -> Bool
  -- ^The result.
intersectsChain maxSegLenSquared space p1 p2
    | maxSegLenSquared <= 2 && qd p1 p2 <= 2 = intersectsChain2 space p1 p2
    | otherwise = any (intersectsSegment segment) segs
  where
      segs = connectedBeads $ beadsInCube space cube
      cube = extendedCube rt . boundingCube . map ($ segment) $ [p, q]
      rt = integerSquareRoot maxSegLenSquared
      segment = Segment p1 p2

-- TODO: kd-tree?
-- |All beads in space contained in the given cube.
beadsInCube :: Space' f -> Cube -> [BeadInfo]
beadsInCube space Cube{..} = mapMaybe getBeadInfo allPositions
  where
    getBeadInfo pos = do
        Bead b <- M.lookup pos space
        pure $ Located pos b
    allPositions = range (minCorner, maxCorner)

-- TODO: złożoność?
-- |All segments between pairs of beads that are neighbours within a chain.
connectedBeads :: [BeadInfo] -> [Segment]
connectedBeads beads = [Segment (b1 ^. position) (b2 ^. position)
                            | (b1, b2) <- distinctPairs beads
                            , chainNeighbours (b1 ^. located) (b2 ^. located)]

-- |Checks if a segment connecting the two given points would intersect with a chain.
-- Assumes that these points are neighbours on the 3-dimensional grid, i. e. the quadrance
-- of the distance between these points equals 1 or 2.
intersectsChain2 :: Space' f -> Vec3 -> Vec3 -> Bool
intersectsChain2 space v1 v2 =
    d /= 1 && case (`M.lookup` space) <$> crossPoss v1 v2 of
                [Just (Bead b1), Just (Bead b2)] -> chainNeighbours b1 b2
                _                                -> False
  where
    d = qd v1 v2

-- |Tests whether the two given beads are connected neighbours on a chain.
chainNeighbours :: BeadSignature -> BeadSignature -> Bool
chainNeighbours b1 b2 = b1 ^. beadChain == b2 ^. beadChain
                     && abs (b1 ^. beadIndexOnChain - b2 ^. beadIndexOnChain) == 1

-- |Returns the segment crossing the segment connecting the two given points.
-- Assumes that the quadrance between these points equals 2.
-- Returns a two-element list for convenience.
crossPoss :: Vec3 -> Vec3 -> [Vec3]
crossPoss (V3 x1 y1 z1) (V3 x2 y2 z2) | x1 == x2 = [V3 x1 y1 z2, V3 x1 y2 z1]
                                      | y1 == y2 = [V3 x1 y1 z2, V3 x2 y1 z1]
                                      | z1 == z2 = [V3 x1 y2 z1, V3 x2 y1 z1]
