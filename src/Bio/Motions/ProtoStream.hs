{- |
Module      : Bio.Motions.ProtoStream
Description : Bindings to libprotostream
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE ForeignFunctionInterface #-}
module Bio.Motions.ProtoStream where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

data HStream
type COffset = CULLong

foreign import ccall "new_stream_reader" new_stream_reader :: IO (Ptr HStream)
foreign import ccall "new_stream_writer" new_stream_writer :: IO (Ptr HStream)
foreign import ccall "new_mmap_reader" new_mmap_reader :: IO (Ptr HStream)
foreign import ccall "delete_stream" delete_stream :: Ptr HStream -> IO ()

foreign import ccall "stream_open" stream_open :: Ptr HStream -> CString -> IO CInt
foreign import ccall "stream_offset_for_kf" stream_offset_for_kf :: Ptr HStream -> CULLong -> IO COffset
foreign import ccall "stream_get_header" stream_get_header :: Ptr HStream -> Ptr CSize -> IO (Ptr ())
foreign import ccall "stream_load_header" stream_load_header :: Ptr HStream -> IO (Ptr ())
foreign import ccall "stream_get_keyframe_data"
    stream_get_keyframe_data :: Ptr HStream -> CULLong -> Ptr CSize -> IO (Ptr ())
