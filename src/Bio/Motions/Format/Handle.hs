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
{-# LANGUAGE TupleSections #-}
module Bio.Motions.Format.Handle
    ( BinaryBackend
    , openBinaryOutput
    , openBinaryInput
    , withBinaryInput
    , seekBinaryKF
    ) where

import Bio.Motions.Format.ProtoStream
import Bio.Motions.Format.DumpSerialisation
import Bio.Motions.Format.DumpDeserialisation
import qualified Bio.Motions.Format.Proto.Header as ProtoHeader

import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Class
import qualified Bio.Motions.Callback.Class as CC
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
import Data.Maybe

import Control.Monad
import Control.Exception(bracket)
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
        pure $ if cur == framesPerKF then
                   PushDump $ \dump callbacks step _ _ -> appendKeyframe state dump callbacks step
               else
                   PushMove $ \move callbacks step _ -> appendDelta state move callbacks step

    closeBackend BinaryBackend{..} = protoClose handle
                >> freeRef keyframeIterator protoFreeKeyframeIterator
                >> freeRef deltaIterator protoFreeDeltaIterator
        where freeRef r f = readIORef r >>= f
    pushLastFrame _ _ _ _ _ = pure ()

-- |Create a 'BinaryBackend'
openBinaryOutput ::
       Int
    -- ^Number of frames per keyframe
    -> OutputSettings
    ->  Dump
    -> IO BinaryBackend
openBinaryOutput framesPerKF OutputSettings{..} dump = do
    handle <- openOutput
    framesSinceLastKF <- newIORef 1
    let mode = Appending
    keyframeIterator <- newIORef nullPtr
    deltaIterator <- newIORef nullPtr
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

appendKeyframe :: BinaryBackend -> Dump -> CC.Callbacks -> StepCounter -> IO ()
appendKeyframe BinaryBackend{..} dump callbacks step = do
    writeIORef framesSinceLastKF 1
    genericAppend handle protoAppendKeyframe $ getKeyframe dump callbacks step

appendDelta :: BinaryBackend -> Move -> CC.Callbacks -> StepCounter -> IO ()
appendDelta BinaryBackend{..} move callbacks step = do
    modifyIORef framesSinceLastKF (+1)
    genericAppend handle protoAppendDelta $ serialiseMove move callbacks step

openBinaryInput :: InputSettings -> IO BinaryBackend
openBinaryInput InputSettings{..} = do
    handle <- case inputFiles of
                [file] -> withCString file protoOpenExisting
                _ -> fail "Specify exactly one binary input file"
    framesPerKF <- fromIntegral <$> protoGetFPKF handle
    framesSinceLastKF <- newIORef 0
    let mode = Reading
    kfIt <- protoIterKeyframes handle
    deltaIterator <- protoIterDeltas kfIt >>= newIORef
    keyframeIterator <- newIORef kfIt
    --------
    valid <- (/= 0) <$> protoValidKeyframeIterator handle kfIt
    unless valid $ error "inv kfi start"
    -----------
    header <- genericGet (protoGetHeader handle) protoFreeHeader id
    return BinaryBackend{..}

withBinaryInput :: InputSettings -> (BinaryBackend -> [String] -> [String] -> IO a) -> IO a
withBinaryInput s f = bracket open closeBackend (\backend -> f backend (getNames backend) (binderTN backend))
    where open = openBinaryInput s 
          getNames = fromMaybe (error "error getting chain names") . getChainNames . header
          binderTN = getBinderTypesNames . header

-- | Seek `i` frames from the beginning and return the Dump at this position. `i` must be divisible by fpkf.
seekBinaryKF :: BinaryBackend -> Int -> IO Dump
seekBinaryKF BinaryBackend{..} i = do
    when (0 /= i `mod` framesPerKF) $ fail "not divisible by fpkf"
    kfi <- readIORef keyframeIterator
    protoAdvanceKeyframeIterator kfi $ fromIntegral (i `div` framesPerKF)
    valid <- (/= 0) <$> protoValidKeyframeIterator handle kfi
    unless valid $ error "out of bounds"
    readIORef deltaIterator >>= protoFreeDeltaIterator
    protoIterDeltas kfi >>= writeIORef deltaIterator
    writeIORef framesSinceLastKF 1
    liftIO $ genericGet (protoGetKeyframe kfi) protoFreeKeyframe $
        fromMaybe (error "invalid dump") . deserialiseDump header

-- |Get a value from a stream using a protostream getter and its associated deallocation function
genericGet :: (Wire msg, ReflectDescriptor msg) =>
       (Ptr (Ptr ()) -> Ptr CSize -> IO ())
       -- ^protostream getter function
    -> (Ptr () -> IO ())
       -- ^ protostream finalizer (i.e. free)
    -> (msg -> b)
       -- ^ deserialising function
    -> IO b
genericGet getter finalizer fun = do
    ptrPtr' <- mallocForeignPtr
    sizePtr' <- mallocForeignPtr
    withForeignPtr ptrPtr' $ \ptrPtr ->
        withForeignPtr sizePtr' $ \sizePtr -> do
            getter ptrPtr sizePtr
            buf <- peek ptrPtr
            size <- fromIntegral <$> peek sizePtr
            bs <- unsafePackCStringFinalizer (castPtr buf) size $ finalizer buf
            let msg = case messageGet $ BL.fromStrict bs of
                    Right (m, _) -> m
                    Left e -> error $ "protobuf decoding error: " ++ e
            return $ fun msg

instance (MonadIO m, ReadRepresentation m repr) => MoveProducer m repr BinaryBackend where
    getMove BinaryBackend{..} repr score = do
        delta <- liftIO (readIORef deltaIterator)
        kfi <- liftIO (readIORef keyframeIterator)
        valid <- liftIO (protoValidDeltaIterator kfi delta)
        liftIO $ putStrLn "bin getMove"
        ret <- case valid of
                 0 -> do
                    liftIO $ protoAdvanceKeyframeIterator kfi 1
                    liftIO (protoValidKeyframeIterator handle kfi) >>= \case
                        0 -> return Nothing
                        x -> do
                            liftIO $ print x
                            ret <- getDiffed kfi
                            liftIO $ protoFreeDeltaIterator delta
                            liftIO $ protoIterDeltas kfi >>= writeIORef deltaIterator
                            return $ Just ret
                 _ -> do
                    ret <- liftIO $ getDelta delta
                    liftIO $ protoAdvanceDeltaIterator delta
                    return $ Just ret
        case ret of
          Just move -> MakeMove move <$> CC.updateCallback repr score move
          Nothing -> return Stop
      where
        getDelta delta = genericGet (protoGetDelta delta) protoFreeDelta $ \msg ->
            fromMaybe (error "failed to decode Move") $ deserialiseMove msg

        getDiffed kfi = do
            dump' <- liftIO $ genericGet (protoGetKeyframe kfi) protoFreeKeyframe $
                fromMaybe (error "failed to load Dump") . deserialiseDump header
            dump <- makeDump repr
            case diffDumps dump dump' of
              Right m -> return m
              Left e -> error $ "dump difference error: " ++ e
    {-# INLINABLE getMove #-}
