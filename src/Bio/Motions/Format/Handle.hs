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
import Bio.Motions.Format.DumpDeserialisation

import Bio.Motions.Representation.Dump
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
    -- |Frames per keyframe
    , currentKFFrames :: IORef Int
    -- |Frames written since last keyframe
    , settings :: OutputSettings
    }

instance OutputBackend BinaryBackend where
    getNextPush state@BinaryBackend{..} = do
        cur <- readIORef currentKFFrames
        if cur == framesPerKF then
                              return $ PushDump $ appendKeyframe state
                              else
                              return $ PushMove $ appendDelta state

    bClose BinaryBackend{..} = protoClose handle
    pushLastFrame _ _ = pure ()

openOutput ::
       Int
    -- ^Number of frames per keyframe
    -> OutputSettings
    -> Dump
    -> IO OutputHandle
openOutput fpkf OutputSettings{..} dump = withCString path $ \cPath ->
    unsafeUseAsCStringLen bytes $ \(ptr, len) ->
        protoOpenNew cPath (fromIntegral fpkf) (castPtr ptr) (fromIntegral len)
    where header = getHeader simulationName simulationDescription chainNames dump
          bytes = BL.toStrict . messagePut $ header
          path = outputPrefix ++ ".bin"

-- |Create a 'BinaryBackend'
openBinaryOutput :: Int -> OutputSettings -> Dump -> IO BinaryBackend
openBinaryOutput fpkf settings dump = do
    handle <- openOutput fpkf settings dump
    let framesPerKF = fpkf
    currentKFFrames <- newIORef 0
    let st = BinaryBackend{..}
    appendKeyframe st dump
    return st

genericAppend :: (ReflectDescriptor msg, Wire msg) =>
                 OutputHandle -> (Ptr HStream -> Ptr () -> CSize -> IO ()) -> msg -> IO ()
genericAppend stream f msg =
    unsafeUseAsCStringLen bytes $ \(ptr, len) ->
        f stream (castPtr ptr) (fromIntegral len)
    --TODO toStrict is slow
    where bytes = BL.toStrict . messagePut $ msg

appendKeyframe :: BinaryBackend -> Dump -> IO ()
appendKeyframe BinaryBackend{..} dump = do
    writeIORef currentKFFrames 1
    genericAppend handle protoAppendKeyframe . getKeyframe $ dump

-- TODO callbacks
appendDelta :: BinaryBackend -> Move -> IO ()
appendDelta BinaryBackend{..} m = do
    modifyIORef currentKFFrames (+1)
    genericAppend handle protoAppendDelta . serialiseMove $ m
