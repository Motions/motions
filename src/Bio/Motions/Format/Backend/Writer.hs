{- |
Module      : Bio.Motions.Format.Backend.Writer
Description : OutputBackend instance for binary format
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Bio.Motions.Format.Backend.Writer
    ( BinaryWriter
    , openBinaryOutput
    ) where

import Bio.Motions.Format.ProtoStream
import Bio.Motions.Format.DumpSerialisation
import Bio.Motions.Format.Backend.Common
import Bio.Motions.Representation.Dump
import qualified Bio.Motions.Callback.Class as CC
import Bio.Motions.Types
import Bio.Motions.Output

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Data.ByteString.Unsafe
import Data.IORef
import qualified Data.ByteString.Lazy as BL
import Text.ProtocolBuffers
import Control.Monad.State.Strict

newtype BinaryWriter = BinaryWriter { getBackend :: BinaryBackend }

instance OutputBackend BinaryWriter where
    getNextPush (BinaryWriter backend@BinaryBackend {..}) = do
        framesSinceLastKF' <- liftIO $ framesSinceLastKF <$> readIORef protoState
        pure $ if framesSinceLastKF' == framesPerKF
            then PushDump $ \dump callbacks step _ _ -> appendKeyframe dump callbacks step backend
            else PushMove $ \move callbacks step _ -> appendDelta move callbacks step backend

    closeBackend = closeBinaryBackend . getBackend
    pushLastFrame _ _ _ _ _ = pure ()

-- |Create a 'BinaryWriter'
openBinaryOutput ::
       Int
    -- ^Number of frames per keyframe
    -> OutputSettings
    -> Dump
    -> IO BinaryWriter
openBinaryOutput framesPerKF OutputSettings {..} dump = do
    handle <- openOutput
    -- Hack. We want the first getNextPush to return PushDump, so
    -- we set framesSinceLastKF to what the if statement is expecting.
    protoState <- newIORef initState
    pure $ BinaryWriter BinaryBackend {..}
  where
    initState :: ProtoState
    initState = ProtoState { framesSinceLastKF = framesPerKF
                           , keyframeIterator = nullPtr
                           , deltaIterator = nullPtr
                           }

    openOutput = withCString path $ \cPath ->
        unsafeUseAsCStringLen bytes $ \(ptr, len) ->
            protoOpenNew cPath (fromIntegral framesPerKF) (castPtr ptr) (fromIntegral len)
    header = getHeader simulationName simulationDescription binderTypesNames chainNames dump
    bytes = BL.toStrict . messagePut $ header
    path = outputPrefix ++ ".bin"

-- |Append a protobuf value to a stream, using a protostream function
genericAppend :: (ReflectDescriptor msg, Wire msg) =>
                    ProtoHandle
                    -- ^Handle to a protostream object
                 -> (ProtoHandle -> Ptr () -> CSize -> IO ())
                    -- ^libprotostream handler function
                 -> msg
                    -- ^Protobuf message to write
                 -> IO ()
genericAppend stream f msg =
    unsafeUseAsCStringLen bytes $ \(ptr, len) ->
        f stream (castPtr ptr) (fromIntegral len)
    --TODO toStrict is slow
  where bytes = BL.toStrict . messagePut $ msg

appendKeyframe :: Dump -> CC.Callbacks -> StepCounter -> BinaryBackend -> IO ()
appendKeyframe dump callbacks step BinaryBackend {..} = do
    modifyIORef protoState $ \s -> s { framesSinceLastKF = 1 }
    liftIO $ genericAppend handle protoAppendKeyframe $ getKeyframe dump callbacks step

appendDelta :: Move -> CC.Callbacks -> StepCounter -> BinaryBackend -> IO ()
appendDelta move callbacks step BinaryBackend {..} = do
    modifyIORef protoState $ \s -> s { framesSinceLastKF = framesSinceLastKF s + 1 }
    liftIO $ genericAppend handle protoAppendDelta $ serialiseMove move callbacks step
