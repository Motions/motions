{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.Format.Handle where

import Bio.Motions.Format.ProtoStream
import Bio.Motions.Format.DumpSerialisation
import Bio.Motions.Format.DumpDeserialisation

import Bio.Motions.Representation.Dump
import Bio.Motions.Types

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy as BL
import Text.ProtocolBuffers


-- TODO: move this type somewhere else (and share with PDB)
data OutputSettings = OutputSettings { outputFile :: FilePath,
                                     framesPerKF :: Int,
                                     simulationName :: String,
                                     simulationDescription :: String,
                                     chainNames :: [String]
                                     }

newtype OutputHandle = OutputHandle (Ptr HStream)

openOutput :: OutputSettings -> Dump -> IO OutputHandle
openOutput OutputSettings{..} dump = withCString outputFile $ \cPath ->
    unsafeUseAsCStringLen bytes $ \(ptr, len) ->
        OutputHandle <$> protoOpenNew cPath (fromIntegral framesPerKF) (castPtr ptr) (fromIntegral len)
    where header = getHeader simulationName simulationDescription chainNames dump
          bytes = BL.toStrict . messagePut $ header

close :: OutputHandle -> IO ()
close (OutputHandle ptr) = protoClose ptr

genericAppend :: (ReflectDescriptor msg, Wire msg) =>
                 OutputHandle -> (Ptr HStream -> Ptr () -> CSize -> IO ()) -> msg -> IO ()
genericAppend (OutputHandle stream) f msg =
    unsafeUseAsCStringLen bytes $ \(ptr, len) -> 
        f stream (castPtr ptr) (fromIntegral len)
    --TODO toStrict is slow
    where bytes = BL.toStrict . messagePut $ msg

appendKeyframe :: OutputHandle -> Dump -> IO ()
appendKeyframe handle = genericAppend handle protoAppendKeyframe . getKeyframe

-- TODO callbacks
appendDelta :: OutputHandle -> Move -> IO ()
appendDelta handle = genericAppend handle protoAppendDelta . serialiseMove
