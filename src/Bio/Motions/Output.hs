module Bio.Motions.Output where

import Bio.Motions.Types
import Bio.Motions.Representation.Dump

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
