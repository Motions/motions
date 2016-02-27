{- |
Module      : Bio.Motions.Callback.GyrationRadius
Description : Gyration radius callback
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Bio.Motions.Callback.GyrationRadius(GyrationRadius(..)) where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Callback.Class
import Bio.Motions.Representation.Class
import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe
import Data.MonoTraversable
import Data.Foldable
import Linear

newtype GyrationRadius = GyrationRadius [Double]
    deriving(Eq, Show)

instance Callback 'Post GyrationRadius where
    callbackName _ = "Gyration Radius"

    runCallback repr = GyrationRadius <$> do
      chains_count <- getNumberOfChains repr
      forM [0..chains_count - 1] $ gyrationRadius repr

    updateCallback repr (GyrationRadius prev) m@Move{..} = GyrationRadius <$> do
        maybe_chain_index <- findChainWith moveFrom repr
        if isNothing maybe_chain_index then return prev else do
            let Just chain_index = maybe_chain_index
            change <- gyrationRadiusChange m repr
            return [if idx == chain_index then value + change else value | (idx, value) <- zip [0..] prev]

-- |Computes gyration radius of a given chain
gyrationRadius :: (Monad m, ReadRepresentation m repr) => repr -> Int -> m Double
gyrationRadius repr chain_index = do
    sum_of_distances <- getChain repr chain_index $ return . sumOfDistances
    chain_length <- getChain repr chain_index $ return . fromIntegral . olength
    return $ sum_of_distances / countPairs chain_length

-- |Computes the change of the gyration radius in the moved chain
gyrationRadiusChange :: (Monad m, ReadRepresentation m repr) => Move -> repr -> m Double
gyrationRadiusChange Move{..} repr = do
    maybe_chain_index <- findChainWith moveFrom repr
    if isNothing maybe_chain_index then return 0 else do
        let Just chain_index = maybe_chain_index
        to_substract <- getChain repr chain_index $ return . distancesToAll moveFrom
        to_add <- getChain repr chain_index $ return . distancesToAll (moveFrom + moveDiff)
        let diff_len = sqrt $ fromIntegral $ quadrance moveDiff
        chain_length <- getChain repr chain_index $ return . fromIntegral . olength
        return $ (to_add - diff_len - to_substract) / countPairs chain_length


-- |Returns index of the chain with a bead with given position
findChainWith :: (Monad m, ReadRepresentation m repr) => Vec3 -> repr -> m (Maybe Int)
findChainWith pos repr = do
    chains_count <- getNumberOfChains repr
    bool_values <- forM [0..chains_count - 1] (\n ->
      getChain repr n $ return . hasBeadWithPosition pos)
    return $ listToMaybe [x | (x,True) <- zip [0..] bool_values]


-- |Computes sum of distances between all pairs of atoms in a chain
sumOfDistances :: (MonoTraversable c, Element c ~ BeadInfo) => c -> Double
sumOfDistances chain = ofoldl' (\result bead ->
                               distancesToAll (bead ^. position) chain + result) 0 chain / 2.0

-- |Computes sum of distances between given atom and all other atoms in the chain
distancesToAll :: (MonoTraversable c, Element c ~ BeadInfo) => Vec3 -> c -> Double
distancesToAll start = ofoldl' (\result bead ->
                               result + sqrt (fromIntegral $ quadrance $ (bead ^. position) - start)) 0

-- |Checks if a given chain has a bead with given position
hasBeadWithPosition :: (MonoTraversable c, Element c ~ BeadInfo) => Vec3 -> c -> Bool
hasBeadWithPosition pos = ofoldl' (\result bead -> result || bead ^. position == pos) False

-- |Number of pairs
countPairs :: Double -> Double
countPairs x = x * (x - 1) / 2
