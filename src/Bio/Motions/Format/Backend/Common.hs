{- |
Module      : Bio.Motions.Format.Backend.Common
Description : BinaryBackend data definition
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
module Bio.Motions.Format.Backend.Common where

import Bio.Motions.Format.ProtoStream
import qualified Bio.Motions.Format.Proto.Header as ProtoHeader

import Foreign.Ptr

type ProtoHandle = Ptr HStream

data BinaryBackend = BinaryBackend
    { handle :: ProtoHandle
    , framesPerKF :: Int
    -- ^Frames per keyframe
    , framesSinceLastKF :: Int
    -- ^Frames written/read since last keyframe (including that keyframe)
    , keyframeIterator :: Ptr HKeyframeIterator
    , deltaIterator :: Ptr HDeltaIterator
    , header :: ProtoHeader.Header
    }

closeBinaryBackend :: BinaryBackend -> IO ()
closeBinaryBackend BinaryBackend {..} = do
    protoClose handle
    protoFreeKeyframeIterator keyframeIterator
    protoFreeDeltaIterator deltaIterator
