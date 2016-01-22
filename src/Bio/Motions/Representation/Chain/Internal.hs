{- |
Module      : Bio.Motions.Representation.Chain.Internal
Description : Contains the internal definitions for the 'Pure Chain Representation'.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Bio.Motions.Representation.Chain.Internal where

import Bio.Motions.Types
import Bio.Motions.Representation.Class
import qualified Bio.Motions.Representation.Dump as D
import Control.Lens
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Maybe
import Data.MonoTraversable
import qualified Data.Sequences as DS
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Linear

type Space = M.Map Vec3 Atom

data PureChainRepresentation = PureChainRepresentation
    { space :: !Space
    , binders :: !(V.Vector BinderInfo)
    , beads :: !(V.Vector BeadInfo)
    -- ^ Beads from all chains
    , chainIndices :: !(U.Vector Int)
    -- ^ Indices of first atoms of successive chains in the 'beads' vector,
    -- with an additional @'V.length' 'beads'@ at the end -- see 'getChain''.
    , radius :: !Int
    -- ^ Radius of the bounding sphere
    , beadKinds :: !(V.Vector EnergyVector)
    }

instance Applicative m => ReadRepresentation m PureChainRepresentation where
    getBinders PureChainRepresentation{..} f = f binders
    {-# INLINE getBinders #-}

    getNumberOfChains PureChainRepresentation{..} = pure $ U.length chainIndices - 1
    {-# INLINE getNumberOfChains #-}

    getChain repr ix f = f $ getChain' repr ix
    {-# INLINE getChain #-}

    getAtomAt pos PureChainRepresentation{..} = pure $ M.lookup pos space
    {-# INLINE getAtomAt #-}

instance Applicative m => Representation m PureChainRepresentation where
    loadDump dump = pure PureChainRepresentation
        { binders = V.fromList $ D.binders dump
        , beads = V.fromList $ concat $ D.chains dump
        , chainIndices = U.fromList $ scanl' (+) 0 $ map length $ D.chains dump
        , space = M.fromList $
                      [(binderPosition b, Binder b) | b <- D.binders dump]
                   ++ [(beadPosition   b, Bead   b) | b <- concat (D.chains dump)]
        , radius = D.radius dump
        , beadKinds = V.fromList $ D.beadKinds dump
        }

    makeDump repr = pure D.Dump
        { binders = V.toList $ binders repr
        , chains = V.toList . getChain' repr <$> [0..U.length (chainIndices repr) - 2]
        , radius = radius repr
        , beadKinds = V.toList $ beadKinds repr
        }

    generateMove repr@PureChainRepresentation{..} = do
        moveBinder <- getRandom
        if moveBinder then
            pick binders Nothing
        else
            pick beads $ Just $ illegalBeadMove repr
      where
        -- |Pick a random move of some atom in a sequence
        pick :: _  -- Under some cumbersome constraints...
            => s -- ^The sequence of atoms
            -> t (Move -> Element s -> Bool) -- ^A 'Traversable' of additional move constraints
            -> m Move
        pick xs constraints = do
            x <- getRandomElement xs
            d <- getRandomElement legalMoves
            let pos = x ^. position
                pos' = pos + d
            guard $ not $ M.member pos' space
            let m = Move pos d
            forM_ constraints $ \c -> guard $ not $ c m x
            pure m

    performMove (MoveFromTo from to) repr
        | Binder binderInfo <- atom = pure $
            let Just idx = V.elemIndex binderInfo $ binders repr
            in  (repr { space = space'
                      , binders = binders repr V.// [(idx, binderInfo & position .~ to)]
                      }, [])
        | Bead beadInfo <- atom = pure
            (repr { space = space'
                  , beads = beads repr V.// [(beadAtomIndex beadInfo, beadInfo & position .~ to)]
                  }, [])
      where
        atom = space repr M.! from
        atom' = atom & position .~ to
        space' = M.insert to atom' $ M.delete from $ space repr

-- |Picks a random element from a 'DS.IsSequence', assuming that its indices form
-- a continuous range from 0 to @'olength' s - 1@.
getRandomElement :: (MonadRandom m, DS.IsSequence s, DS.Index s ~ Int) => s -> m (Element s)
getRandomElement s = DS.unsafeIndex s <$> getRandomR (0, olength s - 1)

-- |The legal moves an atom may make
legalMoves :: V.Vector Vec3
legalMoves = V.fromList [v | [x, y, z] <- replicateM 3 [-1, 0, 1],
                             let v = V3 x y z,
                             quadrance v `elem` [1, 2]]

-- |The pairs of local neighbours of a bead
localNeighbours :: BeadInfo -> PureChainRepresentation -> [(Vec3, Vec3)]
localNeighbours info repr = zip positions $ tail positions
  where
    ix = beadIndexOnChain info
    chain = getChain' repr $ beadChain info
    neighbours = catMaybes [ DS.index chain $ ix - 1
                           , Just info
                           , DS.index chain $ ix + 1
                           ]
    positions = view position <$> neighbours

-- |Checks if a segment connecting the two given points would intersect with a chain.
-- Assumes that these points are neighbours on the 3-dimensional grid, i. e. the quadrance
-- of the distance between these points equals 1 or 2.
intersectsChain :: PureChainRepresentation -> Vec3 -> Vec3 -> Bool
intersectsChain PureChainRepresentation{..} v1@(V3 x1 y1 z1) v2@(V3 x2 y2 z2) =
    d /= 1 && case (`M.lookup` space) <$> crossPoss of
                [Just (Bead b1), Just (Bead b2)] -> chainNeighbours b1 b2
                _                                -> False
  where
    d = qd v1 v2
    crossPoss | x1 == x2 = [V3 x1 y1 z2, V3 x1 y2 z1]
              | y1 == y2 = [V3 x1 y1 z2, V3 x2 y1 z1]
              | z1 == z2 = [V3 x1 y2 z1, V3 x2 y1 z1]
    chainNeighbours b1 b2 = beadChain b1 == beadChain b2
                         && abs (beadIndexOnChain b1 - beadIndexOnChain b2) == 1

illegalBeadMove :: PureChainRepresentation -> Move -> BeadInfo -> Bool
illegalBeadMove repr Move{..} bead = any (uncurry notOk) pairs
  where
    pairs = localNeighbours (bead & position +~ moveDiff) repr
    notOk b1 b2 = wrongQd (qd b1 b2) || intersectsChain repr b1 b2
    wrongQd d = d <= 0 || d > 2

-- |Returns the chain with the specified index.
getChain' :: PureChainRepresentation -> Int -> V.Vector BeadInfo
getChain' PureChainRepresentation{..} ix = V.slice b (e - b) beads
  where
    [b, e] = U.unsafeIndex chainIndices <$> [ix, ix + 1]
