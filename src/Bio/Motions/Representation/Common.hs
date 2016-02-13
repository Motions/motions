{- |
Module      : Bio.Motions.Representation.Common
Description : Common utility functions for operations on simulation state.
License     : MIT
Stability   : experimental
Portability : unportable
-}

module Bio.Motions.Representation.Common where

import Control.Monad
import Control.Lens
import qualified Data.Map as M
import qualified Data.Vector as V
import Linear

import Bio.Motions.Types

type Space' f = M.Map Vec3 (Atom' f)
type Space = Space' Identity

-- |The legal moves an atom may make
legalMoves :: V.Vector Vec3
legalMoves = V.fromList [v | [x, y, z] <- replicateM 3 [-1, 0, 1],
                             let v = V3 x y z,
                             quadrance v `elem` [1, 2]]

-- |Checks if a segment connecting the two given points would intersect with a chain.
-- Assumes that these points are neighbours on the 3-dimensional grid, i. e. the quadrance
-- of the distance between these points equals 1 or 2.
intersectsChain :: Space' f -> Vec3 -> Vec3 -> Bool
intersectsChain space v1@(V3 x1 y1 z1) v2@(V3 x2 y2 z2) =
    d /= 1 && case (`M.lookup` space) <$> crossPoss of
                [Just (Bead b1), Just (Bead b2)] -> chainNeighbours b1 b2
                _                                -> False
  where
    d = qd v1 v2
    crossPoss | x1 == x2 = [V3 x1 y1 z2, V3 x1 y2 z1]
              | y1 == y2 = [V3 x1 y1 z2, V3 x2 y1 z1]
              | z1 == z2 = [V3 x1 y2 z1, V3 x2 y1 z1]
    chainNeighbours b1 b2 = b1 ^. beadChain == b2 ^. beadChain
                         && abs (b1 ^. beadIndexOnChain - b2 ^. beadIndexOnChain) == 1
