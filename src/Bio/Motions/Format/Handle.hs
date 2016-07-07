{- |
Module      : Bio.Motions.Format.Handle
Description : OutputBackend instance for binary format
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.Format.Handle
    ( BinaryBackend
    , openBinaryOutput
    ) where

import Bio.Motions.Format.ProtoStream
import Bio.Motions.Format.DumpSerialisation

import Bio.Motions.Representation.Dump
import Bio.Motions.Callback.Class
import Bio.Motions.Types
import Bio.Motions.Output

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy as BL
import Text.ProtocolBuffers
import Data.IORef

type OutputHandle = Ptr HStream

data BinaryBackend = BinaryBackend
    { handle :: OutputHandle
    , framesPerKF :: Int
    -- ^Frames per keyframe
    , framesSinceLastKF :: IORef Int
    -- ^Frames written since last keyframe (including that keyframe)
    }

instance OutputBackend BinaryBackend where
    getNextPush state@BinaryBackend{..} = do
        cur <- readIORef framesSinceLastKF
        pure $ if cur == framesPerKF then
                   PushDump $ \dump callbacks step _ _ -> appendKeyframe state dump callbacks step
               else
                   PushMove $ \move callbacks step _ -> appendDelta state move callbacks step

    closeBackend BinaryBackend{..} = protoClose handle
    pushLastFrame _ _ _ _ _ = pure ()

-- |Create a 'BinaryBackend'
openBinaryOutput ::
       Int
    -- ^Number of frames per keyframe
    -> OutputSettings
    -> [String]
    -- ^Binder types names
    ->  Dump
    -> [String]
    -- ^Full names of the chains
    -> IO BinaryBackend
openBinaryOutput framesPerKF OutputSettings{..} binderTypesNames dump chainNames = do
    handle <- openOutput
    framesSinceLastKF <- newIORef 0
    let st = BinaryBackend{..}
    appendKeyframe st dump ([], []) 0
    return st
  where
    openOutput = withCString path $ \cPath ->
        unsafeUseAsCStringLen bytes $ \(ptr, len) ->
            protoOpenNew cPath (fromIntegral framesPerKF) (castPtr ptr) (fromIntegral len)
    header = getHeader simulationName simulationDescription binderTypesNames chainNames dump
    bytes = BL.toStrict . messagePut $ header
    path = outputPrefix ++ ".bin"

-- |Append a protobuf value to a stream, using a protostream function
genericAppend :: (ReflectDescriptor msg, Wire msg) =>
                    OutputHandle
                    -- ^Handle to a protostream object
                 -> (Ptr HStream -> Ptr () -> CSize -> IO ())
                    -- ^libprotostream handler function
                 -> msg
                    -- ^Protobuf message to write
                 -> IO ()
genericAppend stream f msg =
    unsafeUseAsCStringLen bytes $ \(ptr, len) ->
        f stream (castPtr ptr) (fromIntegral len)
    --TODO toStrict is slow
  where bytes = BL.toStrict . messagePut $ msg

appendKeyframe :: BinaryBackend -> Dump -> Callbacks -> StepCounter -> IO ()
appendKeyframe BinaryBackend{..} dump callbacks step = do
    writeIORef framesSinceLastKF 1
    genericAppend handle protoAppendKeyframe $ getKeyframe dump callbacks step

appendDelta :: BinaryBackend -> Move -> Callbacks -> StepCounter -> IO ()
appendDelta BinaryBackend{..} move callbacks step = do
    modifyIORef framesSinceLastKF (+1)
    genericAppend handle protoAppendDelta $ serialiseMove move callbacks step
