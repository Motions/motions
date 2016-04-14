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
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
Represents the gyration radius of each chain -- the n-th element
of the list is the gyration radius of the n-th chain. The gyration
radius of a chain is the average parwise distance of its beads.
-}
newtype GyrationRadius = GyrationRadius [Double]
    deriving Eq

instance Show GyrationRadius where
    show (GyrationRadius rs) = show rs

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

-- |Computes the gyration radius of the given chain
gyrationRadius :: (Monad m, ReadRepresentation m repr) => repr -> Int -> m Double
gyrationRadius repr chainIndex = do
    sumOfDistances <- getChain' repr chainIndex sumOfDistances
    chainLength <- getChain' repr chainIndex $ return . fromIntegral . olength
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
    toSubtract <- getChain' repr movedChainIndex $ distancesToAll moveFrom
    toAdd <- getChain repr movedChainIndex $ distancesToAll (moveFrom + moveDiff)
    let diffLen = sqrt $ fromIntegral $ quadrance moveDiff
    chainLength <- getChain' repr movedChainIndex $ return . fromIntegral . olength
    return $ (toAdd - diffLen - toSubtract) / countPairs chainLength

-- |Computes the sum of distances between all pairs of atoms in the given chain
sumOfDistances :: (MonoTraversable c, Element c ~ Located' f a, Wrapper m f) => c -> m Double
sumOfDistances chain = (/2) <$> ofoldlM go 0 chain
  where
    go result bead = do
        pos <- unwrap $ bead ^. wrappedPosition
        (result +) <$> distancesToAll pos chain

-- |Computes the sum of distances between the given position and all atoms in the given chain
distancesToAll :: (MonoTraversable c, Element c ~ Located' f a, Wrapper m f) => Vec3 -> c -> m Double
distancesToAll start = ofoldlM go 0
  where
    go result bead = do
        pos <- unwrap $ bead ^. wrappedPosition
        pure $ result + sqrt (fromIntegral $ qd pos start)

-- |Number of pairs
countPairs :: (Fractional a) => a -> a
countPairs x = x * (x - 1) / 2
