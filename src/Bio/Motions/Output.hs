module Bio.Motions.Output where

import Bio.Motions.Types
import Bio.Motions.Representation.Dump

import Control.Monad
import Control.Exception
import Control.DeepSeq

data OutputSettings = OutputSettings
    { outputPrefix :: FilePath
    , simulationName :: String
    , simulationDescription :: String
    , chainNames :: [String]
    }

class OutputBackend a where
    getNextPush :: a -> IO PushOutputFrame
    forceFullPush :: a -> Dump -> IO () --TODO signal error if impossible?
    bClose :: a -> IO ()

-- TODO PushMove should do callbacks
data PushOutputFrame = PushDump (Dump -> IO ()) | PushMove (Move -> IO ()) | DoNothing

data NullBackend

-- A backend that evaluates all moves, but does no IO. Used for benchmarks.
instance OutputBackend NullBackend where
    getNextPush _ = return $ PushMove $ void . evaluate . force
    forceFullPush _ = void . evaluate . force
    bClose _ = pure ()
