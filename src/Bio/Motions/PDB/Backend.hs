{- |
Module      : Bio.Motions.PDB.Backend
Description : OutputBackend instance for PDB output
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.PDB.Backend where

import Bio.Motions.Common
import Bio.Motions.Output
import Bio.Motions.Types
import Bio.Motions.PDB.Write
import Bio.Motions.PDB.Meta
import Bio.Motions.Representation.Dump

import Control.Lens
import Control.Monad.State
import Data.List
import Data.Maybe
import System.IO

data PDBBackend = PDBBackend
    { pdbHandle :: Handle
    , meta :: PDBMeta
    , metaFile :: FilePath
    -- ^Path to metadata file
    , intermediate :: Bool
    -- ^Whether to write intermediate frames
    }

instance OutputBackend PDBBackend where
    getNextPush st@PDBBackend{..}
        | intermediate = pure $ PushDump $ \dump _ step frame score -> pushPDBStep st dump step frame score
        | otherwise = pure . PushMove $ \_ _ _ _ -> pure ()
    closeBackend PDBBackend{..} = do
        hClose pdbHandle
        withFile metaFile WriteMode $ \h -> writePDBMeta h meta
    pushLastFrame backend dump step frame score
        | intermediate backend = pure ()
        | otherwise = pushPDBStep backend dump step frame score

openPDBOutput ::
    OutputSettings
    -> Dump
    -- ^Current state of the simulation
    -> [String]
    -- ^Names of the chains (in order of their numbers)
    -> [String]
    -- ^Names of the binder types (in order of their numbers)
    -> Bool
    -- ^Use simple PDB names?
    -> Bool
    -- ^Output intermediate steps?
    -> IO PDBBackend
    -- ^Opened PDB backend
openPDBOutput OutputSettings{..} dump chainNames binderTypesNames simplePDB intermediate = do
    let pdbFile = outputPrefix ++ ".pdb"
        laminFile = outputPrefix ++ "-lamin.pdb"
        metaFile = pdbFile ++ ".meta"
        evs = nub . map dumpBeadEV . concat . dumpChains $ dump
        bts = nub . map (^. binderType) . dumpBinders $ dump
        chIds = nub . map (^. beadChain) . concat . dumpIndexedChains $ dump
    when (length chainNames /= length chIds)
        $ error "Fatal error: the number of chain names differs from the number of chains"
    when (length binderTypesNames /= length bts)
        $ error "Fatal error: the number of binder type names differs from the number of binder types"
    let chs = zip chIds chainNames
        mkMeta = if simplePDB then mkSimplePDBMeta else mkPDBMeta
        meta = fromMaybe (error pdbError) $ mkMeta evs (zip bts binderTypesNames) chs
    pdbHandle <- openFile pdbFile WriteMode
    withFile laminFile WriteMode $ \h -> pushPDBLamins h meta dump
    return PDBBackend{..}
  where
    pdbError = "The PDB format can't handle this number of different beads, binders or chains."

-- |Append a step to the output file
pushPDBStep :: (Show score) => PDBBackend -> Dump -> StepCounter -> FrameCounter -> score -> IO ()
pushPDBStep PDBBackend{..} dump' step frame score = do
    let frameHeader = StepHeader { headerSeqNum = frame
                                 , headerStep = step
                                 , headerTitle = "chromosome;bonds=" ++ show score
                                 }
    liftIO $ writePDB pdbHandle frameHeader meta dump >> hPutStrLn pdbHandle "END"
  where
    dump = removeLamins dump'
    removeLamins d = d { dumpBinders = filter notLamin $ dumpBinders d }
    notLamin b = b ^. binderType /= laminType

-- |Write the lamin file to disk
pushPDBLamins :: Handle -> PDBMeta -> Dump -> IO ()
pushPDBLamins handle pdbMeta dump' = do
    let dump = filterLamins dump'
    liftIO $ writePDB handle LaminHeader pdbMeta dump >> hPutStrLn handle "END"
  where
    filterLamins d = Dump { dumpBinders = filter isLamin $ dumpBinders d, dumpChains = [] }
    isLamin b = b ^. binderType == laminType
