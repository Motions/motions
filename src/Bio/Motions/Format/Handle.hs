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
import Data.IORef


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
                              return $ PushMove $ appendDelta state
                              else
                              return $ PushDump $ appendKeyframe state

    bClose (BinaryBackend ProtostreamState{..}) = protoClose handle

openOutput :: Int -> OutputSettings -> Dump -> IO OutputHandle
openOutput fpkf OutputSettings{..} dump = withCString path $ \cPath ->
    unsafeUseAsCStringLen bytes $ \(ptr, len) ->
        protoOpenNew cPath (fromIntegral fpkf) (castPtr ptr) (fromIntegral len)
    where header = getHeader simulationName simulationDescription chainNames dump
          bytes = BL.toStrict . messagePut $ header
          path = outputPrefix ++ ".bin"

createBinaryBackend :: Int -> OutputSettings -> Dump -> IO BinaryBackend
createBinaryBackend fpkf settings dump = do
    handle <- openOutput fpkf settings dump
    let framesPerKF = fpkf
    currentKFFrames <- newIORef 0
    return $ BinaryBackend ProtostreamState{..}

genericAppend :: (ReflectDescriptor msg, Wire msg) =>
                 OutputHandle -> (Ptr HStream -> Ptr () -> CSize -> IO ()) -> msg -> IO ()
genericAppend stream f msg =
    unsafeUseAsCStringLen bytes $ \(ptr, len) -> 
        f stream (castPtr ptr) (fromIntegral len)
    --TODO toStrict is slow
    where bytes = BL.toStrict . messagePut $ msg

appendKeyframe :: ProtostreamState -> Dump -> IO ()
appendKeyframe ProtostreamState{..} dump = do
    writeIORef currentKFFrames 0
    genericAppend handle protoAppendKeyframe . getKeyframe $ dump

-- TODO callbacks
appendDelta :: ProtostreamState -> Move -> IO ()
appendDelta ProtostreamState{..} m = do
    modifyIORef currentKFFrames (+1)
    genericAppend handle protoAppendDelta . serialiseMove $ m
