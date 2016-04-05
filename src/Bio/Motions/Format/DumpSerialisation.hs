{- |
Module      : Bio.Motions.Format.DumpSerialisation
Description : Dump to proto message serialisation
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Bio.Motions.Format.DumpSerialisation where

import qualified Data.Vector.Unboxed as U
import qualified Data.Sequence as S
import Linear
import Text.ProtocolBuffers.Basic

import qualified Bio.Motions.Format.Proto.Header as ProtoHeader
import qualified Bio.Motions.Format.Proto.Keyframe as ProtoKeyframe
import qualified Bio.Motions.Format.Proto.Keyframe.Binder as ProtoBinder
import qualified Bio.Motions.Format.Proto.Callback as ProtoCallback
import Bio.Motions.Format.Proto.Delta
import Bio.Motions.Format.Proto.Keyframe.Chain
import Bio.Motions.Format.Proto.Point
import Bio.Motions.Format.Proto.Header.ChainDescription.BeadDescription.Binding
import Bio.Motions.Format.Proto.Header.ChainDescription.BeadDescription
import Bio.Motions.Format.Proto.Header.ChainDescription
import Bio.Motions.Representation.Dump
import Bio.Motions.Types
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Serialisation

type Callbacks = ([CallbackResult 'Pre], [CallbackResult 'Post])

serialiseMove :: Move -> Callbacks -> Delta
serialiseMove Move{..} cbs = Delta
    { from = Just $ makePoint moveFrom
    , disp = Just $ makePoint moveDiff
    , callbacks = S.fromList $ serialiseCallbacks cbs
    }

getHeader :: String -> String -> [String] -> Dump -> ProtoHeader.Header
getHeader simulationName simulationDescription chainNames Dump{..} = ProtoHeader.Header
    { simulation_name = Just $ uFromString simulationName
    , simulation_description = Just $ uFromString simulationDescription
    , binders_types_count = Just $ fromIntegral $ countBinderTypes dumpChains
    , chains = S.fromList $ zipWith headerSerialiseChain chainNames dumpChains
    }

getKeyframe :: Dump -> Callbacks -> ProtoKeyframe.Keyframe
getKeyframe Dump{..} cbs = ProtoKeyframe.Keyframe
    { binders = S.fromList $ serialiseBinders dumpBinders
    , chains = S.fromList $ map keyframeSerialiseChain dumpChains
    , callbacks = S.fromList $ serialiseCallbacks cbs
    }

headerSerialiseChain :: String -> [DumpBeadInfo] -> ChainDescription
headerSerialiseChain name beads = ChainDescription
    { chain_name = Just $ uFromString name
    , beads = S.fromList [BeadDescription $ S.fromList $ serialiseEnergyVector dumpBeadEV | DumpBeadInfo{..} <- beads]
    }

countBinderTypes :: [[DumpBeadInfo]] -> Int
countBinderTypes ((DumpBeadInfo{..}:_):_) = U.length $ getEnergyVector dumpBeadEV
countBinderTypes _ = error "No chains, or empty chain"

keyframeSerialiseChain :: [DumpBeadInfo] -> Chain
keyframeSerialiseChain beads = Chain{bead_positions =
                            S.fromList [makePoint dumpBeadPosition | DumpBeadInfo{..} <- beads]}

serialiseBinders :: [BinderInfo] -> [ProtoBinder.Binder]
serialiseBinders binderInfos =
    [ProtoBinder.Binder (Just $ fromIntegral binderType) (Just $ makePoint pos) |
        BinderInfo pos (BinderType binderType) <- binderInfos]

serialiseEnergyVector :: EnergyVector -> [Binding]
serialiseEnergyVector (EnergyVector ev) =
    [Binding (Just index) (Just (fromIntegral force)) | (index, force) <- zip [0..] $ U.toList ev]

makePoint :: Vec3 -> Point
makePoint p = Point{..}
    where V3 x y z = Just . fromIntegral <$> p

serialiseCallbacks :: Callbacks -> [ProtoCallback.Callback]
serialiseCallbacks (a, b) = ser a ++ ser b
  where
    ser = map (\(CallbackResult x) -> serialiseCallback (getCallbackName x) x)
