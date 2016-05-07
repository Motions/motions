{- |
Module      : Bio.Motions.Representation.Class
Description : Contains the definition of the 'Dump' class.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Bio.Motions.Representation.Dump where

import Bio.Motions.Common
import Bio.Motions.Types
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import GHC.Generics (Generic)
import Control.DeepSeq

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
    deriving (Eq, Show, Generic, NFData)

-- |Represents a dump of the simulation state
data Dump = Dump
    { dumpBinders :: [BinderInfo] -- ^ A list of binders
    , dumpChains :: [[DumpBeadInfo]] -- ^ A list of chains, each represented as a list of beads
    }
    deriving (Eq, Show, Generic, NFData)

dumpIndexedChains :: Dump -> [[BeadInfo]]
dumpIndexedChains = addIndices . dumpChains

-- |Adds the missing bead indices to 'DumpBeadInfo's making them 'BeadInfo's.
addIndices :: [[DumpBeadInfo]] -> [[BeadInfo]]
addIndices xs = evalState (mapM go $ zip [0..] xs) 0
 where
   go (chainIx, chain) = forM (zip [0..] chain) $ \(ixOnChain, DumpBeadInfo{..}) -> do
     atomIx <- get
     modify (+1)
     pure . Located dumpBeadPosition $ BeadSignature
        { _beadEV = dumpBeadEV
        , _beadAtomIndex = atomIx
        , _beadChain = chainIx
        , _beadIndexOnChain = ixOnChain
        }

dropIndices :: BeadInfo -> DumpBeadInfo
dropIndices b = DumpBeadInfo
    { dumpBeadPosition = b ^. position
    , dumpBeadEV = b ^. beadEV
    }

type Diff = StateT (Maybe Move) (Except String) ()

-- |Performs a difference of two dumps, searching for a single position change.
--  Returns an error if no difference, multiple differences, or any other inconsistencies were found.
--  Otherwise returns the resulting 'Move'.
--  Assumes that both dumps are correct.
diffDumps ::
     Dump
  -- ^The dump from which the move was performed.
  -> Dump
  -- ^The dump after the move was performed.
  -> Either String Move
  -- ^The resulting move or error.
diffDumps d1 d2 =
    case runExcept (execStateT diff Nothing) of
      Left err       -> Left err
      Right (Just m) -> Right m
      Right _        -> Left "No differences found"
  where
    diff :: Diff
    diff = diffBinders >> diffChains

    diffBinders :: Diff
    diffBinders = do
        let bs1 = dumpBinders d1
            bs2 = dumpBinders d2
        when (length bs1 /= length bs2)
            $ throwError "Different binder list lengths"
        -- TODO: write tests for ordered binders
        zipWithM_ diffTwoBinders bs1 bs2

    diffChains :: Diff
    diffChains = do
        let chs1 = dumpChains d1
            chs2 = dumpChains d2
        when (length chs1 /= length chs2)
            $ throwError $ "Different chain counts: " ++ show (length chs1) ++ ", " ++ show (length chs2)
        zipWithM_ diffTwoChains chs1 chs2

    diffTwoChains :: [DumpBeadInfo] -> [DumpBeadInfo] -> Diff
    diffTwoChains ch1 ch2 = do
        when (length ch1 /= length ch2)
            $ throwError $ "Different chain lengths: " ++ show (ch1, length ch1)
                                               ++ ", " ++ show (ch2, length ch2)
        zipWithM_ diffTwoBeads ch1 ch2

    diffTwoBeads :: DumpBeadInfo -> DumpBeadInfo -> Diff
    diffTwoBeads b1 b2 = do
        when (dumpBeadEV b1 /= dumpBeadEV b2)
            $ throwError $ "Beads with the same index but different EVs: " ++ show b1 ++ ", " ++ show b2
        diffPositions (dumpBeadPosition b1) (dumpBeadPosition b2)

    diffTwoBinders :: BinderInfo -> BinderInfo -> Diff
    diffTwoBinders b1 b2 = do
        when (b1 ^. binderType /= b2 ^. binderType)
            $ throwError $ "Binders with the same index but different types: " ++ show b1 ++ ", " ++ show b2
        when (b1 ^. binderType == laminType && b1 ^. position /= b2 ^. position)
            $ throwError $ "A lamin binder has moved: " ++ show b1 ++ ", " ++ show b2
        diffPositions (b1 ^. position) (b2 ^. position)

    diffPositions :: Vec3 -> Vec3 -> Diff
    diffPositions v1 v2 = unless (v1 == v2) $ recordMove $ MoveFromTo v1 v2

    recordMove :: Move -> Diff
    recordMove m = get >>= \case
        Nothing  -> put $ Just m
        Just m'  -> throwError $ "At least two differences occured: " ++ show m ++ ", " ++ show m'
