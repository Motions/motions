{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.PDB.Backend where

import Bio.Motions.Output
import Bio.Motions.Types
import Bio.Motions.PDB.Write
import Bio.Motions.PDB.Meta
import Bio.Motions.Representation.Dump

import Control.Lens
import System.IO
import Control.Monad.State

data PDBState = PDBState
    { pdbHandle :: Handle
    , pdbLaminHandle :: Handle
    }

newtype PDBBackend = PDBBackend PDBState

instance OutputBackend PDBBackend where
    getNextPush (PDBBackend st) = pushPDBStep st

openOutput :: OutputSettings -> Dump -> IO PDBBackend
openOutput OutputSettings{..} dump = do
    pdbHandle <- liftIO $ openFile outputFile WriteMode
    pdbLaminHandle <- liftIO $ openFile pdbLaminFile WriteMode

pushPDBStep :: Handle -> PDBMeta -> IO ()
pushPDBStep handle pdbMeta = do
    st@SimulationState{..} <- get
    {-dump <- removeLamins <$> makeDump repr-}
    let frameHeader = StepHeader { headerSeqNum = frameCounter
                                 , headerStep = stepCounter
                                 , headerTitle = "chromosome;bonds=" ++ show score
                                 }
    liftIO $ writePDB handle frameHeader pdbMeta dump >> hPutStrLn handle "END"
    {-put st { frameCounter = frameCounter + 1 }-}
  {-where-}
    {-removeLamins d = d { dumpBinders = filter notLamin $ dumpBinders d }-}
    {-notLamin b = b ^. binderType /= laminType-}

pushPDBLamins :: Dump -> Handle -> PDBMeta -> IO ()
pushPDBLamins handle pdbMeta = do
    SimulationState{..} <- get
    dump <- filterLamins <$> makeDump repr
    liftIO $ writePDB handle LaminHeader pdbMeta dump >> hPutStrLn handle "END"
  where
    filterLamins d = Dump { dumpBinders = filter isLamin $ dumpBinders d, dumpChains = [] }
    isLamin b = b ^. binderType == laminType
