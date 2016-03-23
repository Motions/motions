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

import Bio.Motions.PDB.Meta(mapChains)  --hack
import Control.Lens
import qualified Data.Map as M
import Data.List(nub)
import Data.Maybe


type OutputHandle = Ptr HStream

data BinaryBackend = BinaryBackend
    { handle :: OutputHandle
    , framesPerKF :: Int
    -- |Frames per keyframe
    , framesSinceLastKF :: IORef Int
    -- |Frames written since last keyframe (including that keyframe)
    }

instance OutputBackend BinaryBackend where
    getNextPush state@BinaryBackend{..} = do
        cur <- readIORef framesSinceLastKF
        return $ if cur == framesPerKF then
                      PushDump $ appendKeyframe state
                      else
                      PushMove $ appendDelta state

    closeBackend BinaryBackend{..} = protoClose handle
    pushLastFrame _ _ = pure ()

-- |Create a 'BinaryBackend'
openBinaryOutput ::
       Int
    -- ^Number of frames per keyframe
    -> OutputSettings
    -> Dump
    -> IO BinaryBackend
openBinaryOutput framesPerKF OutputSettings{..} dump = do
    handle <- openOutput
    framesSinceLastKF <- newIORef 0
    let st = BinaryBackend{..}
    appendKeyframe st dump
    return st
  where
    openOutput = withCString path $ \cPath ->
      unsafeUseAsCStringLen bytes $ \(ptr, len) ->
        protoOpenNew cPath (fromIntegral framesPerKF) (castPtr ptr) (fromIntegral len)
    header = getHeader simulationName simulationDescription chainNames dump
    bytes = BL.toStrict . messagePut $ header
    path = outputPrefix ++ ".bin"
    -- ugly hack follows, TODO fix chain names
    chs = nub . map (^. beadChain) . concat . dumpIndexedChains $ dump
    chMap = fromMaybe (error "unable to create chain names") (mapChains chs)
    chainNames = (:[]) <$> ((chMap M.!) <$> chs)

genericAppend :: (ReflectDescriptor msg, Wire msg) =>
                 OutputHandle -> (Ptr HStream -> Ptr () -> CSize -> IO ()) -> msg -> IO ()
genericAppend stream f msg =
    unsafeUseAsCStringLen bytes $ \(ptr, len) ->
        f stream (castPtr ptr) (fromIntegral len)
    --TODO toStrict is slow
    where bytes = BL.toStrict . messagePut $ msg

appendKeyframe :: BinaryBackend -> Dump -> IO ()
appendKeyframe BinaryBackend{..} dump = do
    writeIORef framesSinceLastKF 1
    genericAppend handle protoAppendKeyframe . getKeyframe $ dump

-- TODO callbacks
appendDelta :: BinaryBackend -> Move -> IO ()
appendDelta BinaryBackend{..} m = do
    modifyIORef framesSinceLastKF (+1)
    genericAppend handle protoAppendDelta . serialiseMove $ m
