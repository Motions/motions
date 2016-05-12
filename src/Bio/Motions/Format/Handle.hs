{- |
Module      : Bio.Motions.Format.Handle
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
module Bio.Motions.Format.Handle
    ( BinaryBackend
    , openBinaryOutput
    , openBinaryInput
    , seekBinary
    ) where

import Bio.Motions.Format.ProtoStream
import Bio.Motions.Format.DumpSerialisation
import Bio.Motions.Format.DumpDeserialisation
import qualified Bio.Motions.Format.Proto.Header as ProtoHeader

import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class(Callbacks)
import Bio.Motions.Types
import Bio.Motions.Output
import Bio.Motions.Input

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
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
import Control.Monad.IO.Class


type BHandle = Ptr HStream
data Mode = Reading | Appending

data BinaryBackend = BinaryBackend
    { handle :: BHandle
    , framesPerKF :: Int
    -- ^Frames per keyframe
    , framesSinceLastKF :: IORef Int
    -- ^Frames written/read since last keyframe (including that keyframe)
    , mode :: Mode
    , keyframeIterator :: IORef (Ptr HKeyframeIterator)
    , deltaIterator :: IORef (Ptr HDeltaIterator)
    , header :: ProtoHeader.Header
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
    let mode = Appending
    keyframeIterator <- newIORef nullPtr
    deltaIterator <- newIORef nullPtr
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
    let mode = Reading
    kfIt <- protoIterKeyframes handle
    deltaIterator <- protoIterDeltas kfIt >>= newIORef
    keyframeIterator <- newIORef kfIt
    header <- genericGet (protoGetHeader handle) id
    return BinaryBackend{..}

seekBinary :: BinaryBackend -> Int -> IO ()
seekBinary BinaryBackend{..} i = do
    kfi <- protoIterKeyframes handle
    protoAdvanceKeyframeIterator kfi $ fromIntegral (i `div` framesPerKF)
    let deltas = i `mod` framesPerKF
    writeIORef framesSinceLastKF deltas
    unless (deltas == 0) $ do
        pure () --todo


genericGet :: 
    (Wire msg, ReflectDescriptor msg) =>
        (Ptr (Ptr ()) -> Ptr CSize -> IO ()) -> (msg -> b) -> IO b
genericGet getter fun = do
    ptrPtr' <- mallocForeignPtr
    sizePtr' <- mallocForeignPtr
    withForeignPtr ptrPtr' $ \ptrPtr ->
        withForeignPtr sizePtr' $ \sizePtr -> do
            getter ptrPtr sizePtr
            buf <- peek ptrPtr
            size <- fromIntegral <$> peek sizePtr
            bs <- unsafePackCStringFinalizer (castPtr buf) size $ protoFreeDelta buf
            let (Right (msg, _)) = messageGet $ BL.fromStrict bs
            return $ fun msg

instance (MonadIO m, ReadRepresentation m repr) => MoveProducer m repr BinaryBackend where
    getMove BinaryBackend{..} repr score = liftIO (readIORef deltaIterator) >>= \delta ->
        liftIO (readIORef keyframeIterator) >>= \kfi ->
            liftIO (protoValidDeltaIterator kfi delta) >>= \case
                1 -> do
                    ret <- liftIO $ getDelta delta
                    liftIO $ protoAdvanceDeltaIterator delta
                    return $ Just (ret, score)
                0 -> do
                    liftIO $ protoAdvanceKeyframeIterator kfi 1
                    valid <- (/= 0) <$> liftIO (protoValidKeyframeIterator handle kfi)
                    if not valid then
                         return Nothing     --TODO end of file?
                             else do
                                 ret <- getDiffed kfi
                                 liftIO $ protoFreeDeltaIterator delta
                                 liftIO $ protoIterDeltas kfi >>= writeIORef deltaIterator
                                 return $ Just (ret, score)
      where
        getDelta delta = genericGet (protoGetDelta delta) $ \msg ->
            fromJust $ deserialiseMove msg  --TODO just

        getDiffed kfi = do
            dump' <- liftIO $ genericGet (protoGetKeyframe kfi) $ fromJust . deserialiseDump header --TODO just
            dump <- makeDump repr
            let (Right move) = diffDumps dump dump'
            return move
