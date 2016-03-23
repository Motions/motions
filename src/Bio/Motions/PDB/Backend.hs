{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.PDB.Backend where

import Bio.Motions.Common
import Bio.Motions.Output
import Bio.Motions.Types
import Bio.Motions.PDB.Write
import Bio.Motions.PDB.Meta
import Bio.Motions.Representation.Dump

import Control.Lens
import System.IO
import Data.IORef
import Control.Monad.State

data PDBState = PDBState
    { pdbHandle :: Handle
    , pdbLaminHandle :: Handle
    , frameCounter :: IORef Int
    , meta :: PDBMeta
    , metaFile :: FilePath
    }

newtype PDBBackend = PDBBackend PDBState

instance OutputBackend PDBBackend where
    getNextPush (PDBBackend st) = return $ PushDump $ \d -> pushPDBStep st d
    bClose (PDBBackend PDBState{..})= do
        hClose pdbHandle
        hClose pdbLaminHandle
        withFile metaFile WriteMode $ \h -> writePDBMeta h meta


openOutput :: OutputSettings -> Dump -> IO PDBBackend
openOutput OutputSettings{..} dump = do
    let pdbFile = outputPrefix ++ ".pdb"
        laminFile = outputPrefix ++ "-lamin.pdb"
        metaFile = pdbFile ++ ".meta"
    pdbHandle <- openFile pdbFile WriteMode
    pdbLaminHandle <- openFile laminFile WriteMode
    frameCounter <- newIORef 0
    return $ PDBBackend PDBState{..}

pushPDBStep :: PDBState -> Dump -> IO ()
pushPDBStep PDBState{..} dump = do
    {-st@SimulationState{..} <- get-}
    frame <- readIORef frameCounter
    let frameHeader = StepHeader { headerSeqNum = frame
                                 , headerStep = 0   --TODO
                                 , headerTitle = "chromosome;bonds=" ++ show 0 --TODO
                                 }
    liftIO $ writePDB pdbHandle frameHeader meta dump >> hPutStrLn pdbHandle "END"
    {-put st { frameCounter = frameCounter + 1 }-}
  {-where-}
    {-removeLamins d = d { dumpBinders = filter notLamin $ dumpBinders d }-}
    {-notLamin b = b ^. binderType /= laminType-}

pushPDBLamins :: Handle -> PDBMeta -> Dump -> IO ()
pushPDBLamins handle pdbMeta dump' = do
    let dump = filterLamins dump'
    liftIO $ writePDB handle LaminHeader pdbMeta dump >> hPutStrLn handle "END"
  where
    filterLamins d = Dump { dumpBinders = filter isLamin $ dumpBinders d, dumpChains = [] }
    isLamin b = b ^. binderType == laminType
