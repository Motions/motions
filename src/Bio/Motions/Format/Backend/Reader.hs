{- |
Module      : Bio.Motions.Format.Backend.Reader
Description : MoveProducer instance for binary format
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
{-# LANGUAGE ViewPatterns #-}
module Bio.Motions.Format.Backend.Reader
    ( BinaryReader
    , openBinaryInput
    , withBinaryInput
    , seekBinaryKF
    ) where

import Bio.Motions.Format.ProtoStream
import Bio.Motions.Format.DumpDeserialisation
import Bio.Motions.Format.Backend.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Class
import Bio.Motions.Input
import Bio.Motions.Types
import qualified Bio.Motions.Callback.Class as CC

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Data.Maybe
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Exception(bracket)
import Text.ProtocolBuffers
import Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy as BL

newtype BinaryReader = BinaryReader { getBackend :: BinaryBackend }

instance (MonadIO m, ReadRepresentation m repr) => MoveProducer m repr BinaryReader where
    getMove repr score (BinaryReader backend) = do
        valid <- validDeltaIterator backend
        ret <- runMaybeT $
            if valid then do
                ret <- getDelta backend
                advanceDeltaIterator backend
                pure ret
            else do
                advanceKeyframeIterator 1 backend
                guard =<< validKeyframeIterator backend
                resetDeltaIterator backend
                getDiffed
        case ret of
          Just (move, step) -> MakeMove move <$> CC.updateCallback repr score move <*> pure step
          Nothing -> pure Stop
      where
        getDiffed = do
            (dump', step) <- getKeyframe backend
            dump <- lift $ makeDump repr
            case diffDumps dump dump' of
              Right m -> pure (m, step)
              Left e -> error $ "adjacent keyframes don't match: " ++ e
    {-# INLINABLE getMove #-}

openBinaryInput :: InputSettings -> IO BinaryReader
openBinaryInput InputSettings{..} = do
    handle <- case inputFiles of
                [file] -> withCString file protoOpenExisting
                _ -> fail "Specify exactly one binary input file"
    framesPerKF <- fromIntegral <$> protoGetFPKF handle
    protoState <- initState handle >>= newIORef
    header <- genericGet (protoGetHeader handle) protoFreeHeader id
    pure $ BinaryReader BinaryBackend {..}
  where
    initState :: ProtoHandle -> IO ProtoState
    initState handle = do
        let framesSinceLastKF = 1
        keyframeIterator <- protoIterKeyframes handle
        valid <- (/= 0) <$> protoValidKeyframeIterator handle keyframeIterator
        unless valid $ fail "Cannot get iterator for first frame. Is the file empty?"
        deltaIterator <- protoIterDeltas keyframeIterator
        pure ProtoState {..}

withBinaryInput :: InputSettings -> (BinaryReader -> [String] -> [String] -> IO a) -> IO a
withBinaryInput s f =
    bracket open close (\backend -> f backend (getChainNames' backend) (binderTN backend))
  where
    open = openBinaryInput s
    close = closeBinaryBackend . getBackend
    getChainNames' = fromMaybe (error "error getting chain names") . getChainNames . header . getBackend
    binderTN = getBinderTypesNames . header . getBackend

-- | Seek `i` frames from the beginning and return the Dump at this position. `i` must be divisible by fpkf.
seekBinaryKF :: Int -> BinaryReader -> IO (Dump, StepCounter)
seekBinaryKF i (BinaryReader backend@BinaryBackend {..}) = do
    when (0 /= i `mod` framesPerKF) $ fail "not divisible by fpkf"
    advanceKeyframeIterator (i `div` framesPerKF) backend
    valid <- validKeyframeIterator backend
    unless valid $ fail "out of bounds"
    resetDeltaIterator backend
    getKeyframe backend

validDeltaIterator :: MonadIO m => BinaryBackend -> m Bool
validDeltaIterator (protoState -> state) = liftIO $ do
    ProtoState {..} <- readIORef state
    ret <- protoValidDeltaIterator keyframeIterator deltaIterator
    pure $ ret /= 0

validKeyframeIterator :: MonadIO m => BinaryBackend -> m Bool
validKeyframeIterator backend = liftIO $ do
    keyframeIterator' <- fmap keyframeIterator $ readIORef $ protoState backend
    ret <- protoValidKeyframeIterator (handle backend) keyframeIterator'
    pure $ ret /= 0

advanceKeyframeIterator :: MonadIO m => Int -> BinaryBackend -> m ()
advanceKeyframeIterator n (protoState -> state) = liftIO $ do
    keyframeIterator' <- keyframeIterator <$> readIORef state
    protoAdvanceKeyframeIterator keyframeIterator' $ fromIntegral n

advanceDeltaIterator :: MonadIO m => BinaryBackend -> m ()
advanceDeltaIterator (protoState -> state) = liftIO $ do
    deltaIterator' <- deltaIterator <$> readIORef state
    protoAdvanceDeltaIterator deltaIterator'

resetDeltaIterator :: MonadIO m => BinaryBackend -> m ()
resetDeltaIterator (protoState -> state) = liftIO $ do
    ProtoState {..} <- readIORef state
    protoFreeDeltaIterator deltaIterator
    deltaIterator' <- protoIterDeltas keyframeIterator
    writeIORef state ProtoState { framesSinceLastKF = 1
                                , keyframeIterator = keyframeIterator
                                , deltaIterator = deltaIterator'
                                }

getKeyframe :: MonadIO m => BinaryBackend -> m (Dump, StepCounter)
getKeyframe backend = liftIO $ do
    keyframeIterator' <- fmap keyframeIterator $ readIORef $ protoState backend
    genericGet (protoGetKeyframe keyframeIterator') protoFreeKeyframe $
        fromMaybe (error "failed to load keyframe") . deserialiseDump (header backend)

getDelta :: MonadIO m => BinaryBackend -> m (Move, StepCounter)
getDelta (protoState -> state) = liftIO $ do
    deltaIterator' <- deltaIterator <$> readIORef state
    genericGet (protoGetDelta deltaIterator') protoFreeDelta $ \msg ->
        fromMaybe (error "failed to decode Move") $ deserialiseMove msg

-- |Get a value from a stream using a protostream getter and its associated deallocation function
genericGet :: (Wire msg, ReflectDescriptor msg) =>
       (Ptr (Ptr ()) -> Ptr CSize -> IO ())
       -- ^protostream getter function
    -> (Ptr () -> IO ())
       -- ^protostream finalizer (i.e. free)
    -> (msg -> b)
       -- ^deserialising function
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
            pure $ fun msg
