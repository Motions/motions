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
    , openBinaryInput
    ) where

import Bio.Motions.Format.ProtoStream
import Bio.Motions.Format.DumpSerialisation

import Bio.Motions.Representation.Dump
import Bio.Motions.Callback.Class
import Bio.Motions.Types
import Bio.Motions.Output
import Bio.Motions.Input

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
import Control.Monad


type BHandle = Ptr HStream

data BinaryBackend = BinaryBackend
    { handle :: BHandle
    , framesPerKF :: Int
    -- ^Frames per keyframe
    , framesSinceLastKF :: IORef Int
    -- ^Frames written/read since last keyframe (including that keyframe)
    }

instance OutputBackend BinaryBackend where
    getNextPush state@BinaryBackend{..} = do
        cur <- readIORef framesSinceLastKF
        return $ if cur == framesPerKF then
                      PushDump $ \d c _ _ -> appendKeyframe state d c
                      else
                      PushMove $ appendDelta state

    closeBackend BinaryBackend{..} = protoClose handle
    pushLastFrame _ _ _ _ = pure ()

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
    appendKeyframe st dump ([], [])
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

-- |Append a protobuf value to a stream, using a protostream function
genericAppend :: (ReflectDescriptor msg, Wire msg) =>
                    BHandle
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

appendKeyframe :: BinaryBackend -> Dump -> Callbacks -> IO ()
appendKeyframe BinaryBackend{..} dump c = do
    writeIORef framesSinceLastKF 1
    genericAppend handle protoAppendKeyframe $ getKeyframe dump c

appendDelta :: BinaryBackend -> Move -> Callbacks -> IO ()
appendDelta BinaryBackend{..} m c = do
    modifyIORef framesSinceLastKF (+1)
    genericAppend handle protoAppendDelta $ serialiseMove m c


openBinaryInput :: InputSettings -> IO BinaryBackend
openBinaryInput InputSettings{..} = do
    handle <- withCString inputFile protoOpenExisting
    framesPerKF <- fromIntegral <$> protoGetFPKF handle
    framesSinceLastKF <- newIORef 0
    return BinaryBackend{..}


instance InputBackend BinaryBackend where
    openInput = openBinaryInput
    seekInput BinaryBackend{..} i = do
        kfi <- protoIterKeyframes handle
        protoAdvanceKeyframeIterator kfi $ fromIntegral (i `div` framesPerKF)
        let deltas = i `mod` framesPerKF
        writeIORef framesSinceLastKF deltas
        unless (deltas == 0) $ do
            pure () --TODO
    tellInput = undefined
    inputLength = undefined
    getDump = undefined
    getMove = undefined
    closeInput = closeBackend
