{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.Format.Handle
    ( BinaryBackend
    , createBinaryBackend
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


type OutputHandle = Ptr HStream

data ProtostreamState = ProtostreamState
    { handle :: OutputHandle
    , framesPerKF :: Int
    , currentKFFrames :: IORef Int
    , settings :: OutputSettings
    }

newtype BinaryBackend = BinaryBackend ProtostreamState

instance OutputBackend BinaryBackend where
    getNextPush (BinaryBackend state@ProtostreamState{..}) = do
        cur <- readIORef currentKFFrames 
        if cur == framesPerKF then
                              return $ appendDelta state
                              else
                              return $ appendKeyframe state

    bClose (BinaryBackend ProtostreamState{..}) = protoClose handle

openOutput :: OutputSettings -> Dump -> IO OutputHandle
openOutput OutputSettings{..} dump = withCString outputFile $ \cPath ->
    unsafeUseAsCStringLen bytes $ \(ptr, len) ->
        protoOpenNew cPath (fromIntegral framesPerKF) (castPtr ptr) (fromIntegral len)
    where header = getHeader simulationName simulationDescription chainNames dump
          bytes = BL.toStrict . messagePut $ header


genericAppend :: (ReflectDescriptor msg, Wire msg) =>
                 OutputHandle -> (Ptr HStream -> Ptr () -> CSize -> IO ()) -> msg -> IO ()
genericAppend stream f msg =
    unsafeUseAsCStringLen bytes $ \(ptr, len) -> 
        f stream (castPtr ptr) (fromIntegral len)
    --TODO toStrict is slow
    where bytes = BL.toStrict . messagePut $ msg

appendKeyframe :: ProtostreamState -> Dump -> IO ()
appendKeyframe ProtostreamState{..} = do
    writeIORef currentKFFrames 0
    genericAppend handle protoAppendKeyframe . getKeyframe

-- TODO callbacks
appendDelta :: ProtostreamState -> Move -> IO ()
appendDelta ProtostreamState{..} = do
    atomicModifyIORef currentKFFrames (+1)
    genericAppend handle protoAppendDelta . serialiseMove
