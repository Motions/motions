{- |
Module      : Bio.Motions.Representation.Class
Description : Contains the definition of the 'Dump' class.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.Representation.Dump where

import Bio.Motions.Types
import Control.Monad
import Control.Monad.State

-- |A subset of 'BeadInfo'.
--
-- It lacks fields containing various indices of the bead and its chain, because
-- they may be easily computed given a 'Dump'. Removal of those fields has the effect
-- of making certain incorrect 'Dump's unrepresentable (i.e. those with indices differing
-- from the respective list positions).
data DumpBeadInfo = DumpBeadInfo
    { dumpBeadPosition :: Vec3
    , dumpBeadEV :: EnergyVector
    }
    deriving (Eq, Show)

-- |Represents a dump of the simulation state
data Dump = Dump
    { dumpRadius :: Int -- ^ The bounding sphere radius
    , dumpBinders :: [BinderInfo] -- ^ A list of binders (in unspecified order)
    , dumpChains :: [[DumpBeadInfo]] -- ^ A list of chains, each represented as a list of beads
    }

dumpIndexedChains :: Dump -> [[BeadInfo]]
dumpIndexedChains = addIndices . dumpChains

-- |Adds the missing bead indices to 'DumpBeadInfo's making them 'BeadInfo's.
addIndices :: [[DumpBeadInfo]] -> [[BeadInfo]]
addIndices xs = evalState (mapM go $ zip [0..] xs) 0
 where
   go (chainIx, chain) = forM (zip [0..] chain) $ \(ixOnChain, DumpBeadInfo{..}) -> do
     atomIx <- get
     modify (+1)
     pure BeadInfo
        { beadPosition = dumpBeadPosition
        , beadEV = dumpBeadEV
        , beadAtomIndex = atomIx
        , beadChain = chainIx
        , beadIndexOnChain = ixOnChain
        }

dropIndices :: BeadInfo -> DumpBeadInfo
dropIndices BeadInfo{..} = DumpBeadInfo
    { dumpBeadPosition = beadPosition
    , dumpBeadEV = beadEV
    }
