{- |
Module      : Bio.Motions.Callback.GyrationRadius
Description : Gyration radius callback
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.Callback.GyrationRadius(GyrationRadius(..)) where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Callback.Class
import Bio.Motions.Representation.Class
import Control.Lens
import Control.Monad
import Data.MonoTraversable
import Linear

{- |
Represents a gyration radius of each chain -- on n-th position
of the resulting list stands the gyration radius of the n-th chain.
A gyration radius of a chain is an arithmetic mean of the distances
between all pairs of its beads.
-}
newtype GyrationRadius = GyrationRadius [Double]
    deriving(Eq, Show)

instance Callback 'Pre GyrationRadius where
    callbackName _ = "Gyration Radius"

    runCallback repr = GyrationRadius <$> do
        chainsCount <- getNumberOfChains repr
        forM [0..chainsCount - 1] $ gyrationRadius repr

    updateCallback repr (GyrationRadius prev) m@Move{..} = GyrationRadius <$> do
        atom <- getAtomAt moveFrom repr
        case atom of
            Just (Bead b) -> do
                let chainIndex = b ^. beadChain
                change <- gyrationRadiusChange m repr chainIndex
                return $ prev & ix chainIndex +~ change
            _ -> return prev

-- |Computes gyration radius of the given chain
gyrationRadius :: (Monad m, ReadRepresentation m repr) => repr -> Int -> m Double
gyrationRadius repr chainIndex = do
    sumOfDistances <- getChain repr chainIndex $ return . sumOfDistances
    chainLength <- getChain repr chainIndex $ return . fromIntegral . olength
    return $ sumOfDistances / countPairs chainLength

-- |Computes the change of gyration radius in the moved chain
gyrationRadiusChange :: (Monad m, ReadRepresentation m repr) =>
  Move
  -- ^The move to be made
  -> repr
  -- ^The representation
  -> Int
  -- ^Index of the chain to be moved
  -> m Double
gyrationRadiusChange Move{..} repr movedChainIndex = do
    toSubstract <- getChain repr movedChainIndex $ return . distancesToAll moveFrom
    toAdd <- getChain repr movedChainIndex $ return . distancesToAll (moveFrom + moveDiff)
    let diffLen = sqrt $ fromIntegral $ quadrance moveDiff
    chainLength <- getChain repr movedChainIndex $ return . fromIntegral . olength
    return $ (toAdd - diffLen - toSubstract) / countPairs chainLength

-- |Computes sum of distances between all pairs of atoms in the given chain
sumOfDistances :: (MonoTraversable c, HasPosition (Element c)) => c -> Double
sumOfDistances chain = ofoldl' go 0 chain / 2.0
  where go result bead = result + distancesToAll (bead ^. position) chain

-- |Computes sum of distances between the given position and all atoms in the given chain
distancesToAll :: (MonoTraversable c, HasPosition (Element c)) => Vec3 -> c -> Double
distancesToAll start = ofoldl' go 0
  where go result bead = result + sqrt (fromIntegral $ qd (bead ^. position) start)

-- |Number of pairs
countPairs :: (Fractional a) => a -> a
countPairs x = x * (x - 1) / 2