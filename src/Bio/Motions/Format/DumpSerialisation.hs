{- |
Module      : Bio.Motions.Format.DumpSerialisation
Description : Dump to proto message serialisation
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Bio.Motions.Format.DumpSerialisation where

import Control.Monad
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import Linear
import Text.ProtocolBuffers.Basic

import qualified Bio.Motions.Format.Proto.Header as ProtoHeader
import qualified Bio.Motions.Format.Proto.Keyframe as ProtoKeyframe
import qualified Bio.Motions.Format.Proto.Keyframe.Binder as ProtoBinder
import Bio.Motions.Format.Proto.Keyframe.Chain
import Bio.Motions.Format.Proto.Point
import Bio.Motions.Format.Proto.Header.ChainDescription.BeadDescription.Binding
import Bio.Motions.Format.Proto.Header.ChainDescription.BeadDescription
import Bio.Motions.Format.Proto.Header.ChainDescription
import Bio.Motions.Representation.Dump
import Bio.Motions.Types

serialiseDump :: String -> String -> [String] ->  Dump -> (ProtoHeader.Header, ProtoKeyframe.Keyframe)
serialiseDump simulationName simulationDescription chainNames dump =
    (getHeader simulationName simulationDescription chainNames dump, getKeyframe dump)

getHeader :: String -> String -> [String] -> Dump -> ProtoHeader.Header
getHeader simulationName simulationDescription chainNames Dump{..} = ProtoHeader.Header{
    simulation_name = Just $ uFromString simulationName,
    simulation_description = Just $ uFromString simulationDescription,
    binders_types_count = Just $ countBinderTypes dumpChains,
    radius = Just $ fromIntegral dumpRadius,
    chains = S.fromList $ zipWith headerSerialiseChain chainNames dumpChains
    }


getKeyframe :: Dump -> ProtoKeyframe.Keyframe
getKeyframe Dump{..} = ProtoKeyframe.Keyframe{
    binders = S.fromList $ serialiseBinders dumpBinders,
    chains = S.fromList [keyframeSerialiseChain c | c <- dumpChains],
    callbacks = S.fromList [] --TODO Serialisation of callbacks
    }

headerSerialiseChain :: String -> [DumpBeadInfo] -> ChainDescription
headerSerialiseChain name beads = ChainDescription{
    chain_name = Just $ uFromString name,
    beads = S.fromList [BeadDescription $ S.fromList $ serialiseEnergyVector dumpBeadEV | DumpBeadInfo{..} <- beads]
    }

countBinderTypes :: (Num a) => [[DumpBeadInfo]] -> a
countBinderTypes ((DumpBeadInfo{..}:_):_) = fromIntegral $ U.length $ getEnergyVector dumpBeadEV
countBinderTypes _ = error "No chains, or empty chain"

keyframeSerialiseChain :: [DumpBeadInfo] -> Chain
keyframeSerialiseChain beads = Chain{bead_positions =
                            S.fromList [makePoint dumpBeadPosition | DumpBeadInfo{..} <- beads]}

serialiseBinders :: [BinderInfo] -> [ProtoBinder.Binder]
serialiseBinders binderInfos =
    [ProtoBinder.Binder (Just $ fromIntegral binderType) (Just $ makePoint pos) | BinderInfo pos (BinderType binderType) <- binderInfos]

serialiseEnergyVector :: EnergyVector -> [Binding]
serialiseEnergyVector (EnergyVector ev) =
    [Binding (Just index) (Just (fromIntegral force)) | (index, force) <- zip [0..] $ U.toList ev]

makePoint :: Vec3 -> Point
makePoint (V3 x y z) = Point{x = Just $ fromIntegral x,
                             y = Just $ fromIntegral y,
                             z = Just $ fromIntegral z}
