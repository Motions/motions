{- |
Module      : Bio.Motions.Output
Description : Typeclass for output formats
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE Rank2Types #-}
module Bio.Motions.Output where

import Bio.Motions.Types
import Bio.Motions.Representation.Dump
import Bio.Motions.Callback.Class

import Control.Monad
import Control.Exception
import Control.DeepSeq

data OutputSettings = OutputSettings
    { outputPrefix :: FilePath
    -- ^Prefix of output path, i.e. when outputPrefix = "/foo/bar/asdf",
    -- then a backend would write to /foo/bar/asdf.something
    , simulationName :: String
    , simulationDescription :: String
    }

-- |A backend for writing frames to disk.
-- Opening the output may have different arguments depending on the backend,
-- so it is not included here.
class OutputBackend a where
    -- |See 'PushOutputFrame'
    getNextPush :: a -> IO PushOutputFrame
    -- |Gives the backend a 'Dump' of the last frame. This is useful in cases
    -- where it needs special handling, eg. PDB without intermediate frames.
    pushLastFrame :: (Show score) => a -> Dump -> Int -> score -> IO ()
    -- |Close the file(s) and do all neccessary cleanup.
    closeBackend :: a -> IO ()

-- |Tells the engine what kind of frame representation this backend is expecting now.
-- This is useful because generating a 'Dump' is potentially slow, so we avoid it whenever possible.
-- Some backends (e.g. PDB without intermediates) might ignore frames, hence 'DoNothing'.
data PushOutputFrame =
      PushDump (forall score. (Show score) => Dump -> Callbacks -> Int -> score -> IO ())
      -- ^PDB needs the step number (the Int) and current score
      | PushMove (Move -> Callbacks -> IO ())
      | DoNothing

-- |A backend that evaluates all moves, but does no IO. Used for benchmarks.
data NullBackend

instance OutputBackend NullBackend where
    getNextPush _ = return $ PushMove $ curry (void . evaluate . force)
    pushLastFrame _ dump _ _ = void . evaluate . force $ dump
    closeBackend _ = pure ()
