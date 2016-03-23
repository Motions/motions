{- |
Module      : Bio.Motions.ProtoStream
Description : Bindings to libprotostream
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE ForeignFunctionInterface #-}
module Bio.Motions.Format.ProtoStream where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

data HStream
data HKeyframeIterator
data HDeltaIterator

foreign import ccall "protostream_open_existing"
    protoOpenExisting :: CString -> IO (Ptr HStream)

foreign import ccall "protostream_open_new"
    protoOpenNew :: CString -> CUInt -> Ptr () -> CSize -> IO (Ptr HStream)

foreign import ccall "protostream_close"
    protoClose :: Ptr HStream -> IO ()

foreign import ccall "protostream_append_keyframe"
    protoAppendKeyframe :: Ptr HStream -> Ptr () -> CSize -> IO ()

foreign import ccall "protostream_append_delta"
    protoAppendDelta :: Ptr HStream -> Ptr () -> CSize -> IO ()

foreign import ccall "protostream_get_header"
    protoGetHeader :: Ptr HStream -> Ptr (Ptr ()) -> Ptr CSize -> IO ()

foreign import ccall "protostream_free_header"
    protoFreeHeader :: Ptr () -> IO ()

foreign import ccall "protostream_iter_keyframes"
    protoIterKeyframes :: Ptr HStream -> IO (Ptr HKeyframeIterator)

foreign import ccall "protostream_get_keyframe"
    protoGetKeyframe :: Ptr HKeyframeIterator -> Ptr (Ptr ()) -> Ptr CSize -> IO ()

foreign import ccall "protostream_free_keyframe"
    protoFreeKeyframe :: Ptr () -> IO ()

foreign import ccall "protostream_valid_keyframe_iterator"
    protoValidKeyframeIterator :: Ptr HStream -> Ptr HKeyframeIterator -> IO CInt

foreign import ccall "protostream_advance_keyframe_iterator"
    protoAdvanceKeyframeIterator :: Ptr HKeyframeIterator -> IO ()

foreign import ccall "protostream_free_keyframe_iterator"
    protoFreeKeyframeIterator :: Ptr HKeyframeIterator -> IO ()

foreign import ccall "protostream_iter_deltas"
    protoIterDeltas :: Ptr HKeyframeIterator -> IO (Ptr HDeltaIterator)

foreign import ccall "protostream_get_delta"
    protoGetDelta :: Ptr HDeltaIterator -> Ptr (Ptr ()) -> Ptr CSize -> IO ()

foreign import ccall "protostream_free_delta"
    protoFreeDelta :: Ptr () -> IO ()

foreign import ccall "protostream_valid_delta_iterator"
    protoValidDeltaIterator :: Ptr HKeyframeIterator -> Ptr HDeltaIterator -> IO CInt

foreign import ccall "protostream_advance_delta_iterator"
    protoAdvanceDeltaIterator :: Ptr HDeltaIterator -> IO ()

foreign import ccall "protostream_free_delta_iterator"
    protoFreeDeltaIterator :: Ptr HDeltaIterator -> IO ()
