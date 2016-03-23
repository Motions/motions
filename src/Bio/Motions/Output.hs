module Bio.Motions.Output where

import Bio.Motions.Types
import Bio.Motions.Representation.Dump

data OutputSettings = OutputSettings
    { outputFile :: FilePath
    , simulationName :: String
    , simulationDescription :: String
    , chainNames :: [String]
    }

class OutputBackend a where
    getNextPush :: a -> IO PushOutputFrame
    bClose :: a -> IO ()

-- TODO PushMove should do callbacks
data PushOutputFrame = PushDump (Dump -> IO ()) | PushMove (Move -> IO ()) | DoNothing
