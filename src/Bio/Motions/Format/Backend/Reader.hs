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
import qualified Bio.Motions.Callback.Class as CC
import Bio.Motions.Input
import Bio.Motions.Types

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy as BL
import Text.ProtocolBuffers
import Data.Maybe
import Control.Monad
import Control.Monad.State.Strict
import Control.Exception(bracket)

newtype BinaryReader = BinaryReader { getBackend :: BinaryBackend }

instance (MonadIO m, ReadRepresentation m repr) => MoveProducer m repr BinaryReader where
    getMove repr score = do
        BinaryBackend{..} <- gets getBackend
        valid <- liftIO $ protoValidDeltaIterator keyframeIterator deltaIterator
        ret <- case valid of
            0 -> do
                liftIO $ protoAdvanceKeyframeIterator keyframeIterator 1
                liftIO (protoValidKeyframeIterator handle keyframeIterator) >>= \case
                    0 -> pure Nothing
                    _ -> getDiffed keyframeIterator header >>= \ret -> do
                        deltaIterator' <- liftIO $ do
                            protoFreeDeltaIterator deltaIterator
                            protoIterDeltas keyframeIterator
                        modify $ \(BinaryReader b) -> BinaryReader b { deltaIterator = deltaIterator' }
                        pure $ Just ret
            _ -> liftIO $ do
                ret <- getDelta deltaIterator
                protoAdvanceDeltaIterator deltaIterator
                pure $ Just ret
        case ret of
          Just (move, step) -> MakeMove move <$> lift (CC.updateCallback repr score move) <*> pure step
          Nothing -> pure Stop
      where
        getDelta deltaIterator = genericGet (protoGetDelta deltaIterator) protoFreeDelta $ \msg ->
            fromMaybe (error "failed to decode Move") $ deserialiseMove msg

        getDiffed keyframeIterator header = do
            (dump', step) <- liftIO $ genericGet (protoGetKeyframe keyframeIterator) protoFreeKeyframe $
                fromMaybe (error "failed to load keyframe") . deserialiseDump header
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
    let framesSinceLastKF = 1
    kfIt <- protoIterKeyframes handle
    deltaIterator <- protoIterDeltas kfIt
    let keyframeIterator = kfIt
    valid <- (/= 0) <$> protoValidKeyframeIterator handle kfIt
    unless valid $ fail "Cannot get iterator for first frame. Is the file empty?"
    header <- genericGet (protoGetHeader handle) protoFreeHeader id
    pure $ BinaryReader BinaryBackend {..}

withBinaryInput :: InputSettings -> (BinaryReader -> [String] -> [String] -> IO a) -> IO a
withBinaryInput s f =
    bracket open close (\backend -> f backend (getChainNames' backend) (binderTN backend))
  where
    open = openBinaryInput s
    close = closeBinaryBackend . getBackend
    getChainNames' = fromMaybe (error "error getting chain names") . getChainNames . header . getBackend
    binderTN = getBinderTypesNames . header . getBackend

-- | Seek `i` frames from the beginning and return the Dump at this position. `i` must be divisible by fpkf.
seekBinaryKF :: Int -> StateT BinaryReader IO (Dump, StepCounter)
seekBinaryKF i = do
    BinaryBackend {..} <- gets getBackend
    deltaIterator' <- liftIO $ do
        when (0 /= i `mod` framesPerKF) $ fail "not divisible by fpkf"
        protoAdvanceKeyframeIterator keyframeIterator $ fromIntegral (i `div` framesPerKF)
        valid <- (/= 0) <$> protoValidKeyframeIterator handle keyframeIterator
        unless valid $ fail "out of bounds"
        protoFreeDeltaIterator deltaIterator
        protoIterDeltas keyframeIterator
    modify $ \(BinaryReader b) -> BinaryReader b { deltaIterator = deltaIterator'
                                                 , framesSinceLastKF = 1
                                                 }
    liftIO $ genericGet (protoGetKeyframe keyframeIterator) protoFreeKeyframe $
        fromMaybe (error "invalid keyframe") . deserialiseDump header

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
