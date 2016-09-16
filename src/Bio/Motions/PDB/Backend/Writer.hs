{- |
Module      : Bio.Motions.PDB.Backend.Writer
Description : OutputBackend instance for PDB
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Bio.Motions.PDB.Backend.Writer where

import Bio.Motions.Common
import Bio.Motions.Output
import Bio.Motions.Types
import Bio.Motions.PDB.Write
import Bio.Motions.PDB.Meta
import Bio.Motions.Representation.Dump

import Control.Lens
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import System.IO

data PDBWriter = PDBWriter
    { pdbHandle :: Handle
    , meta :: PDBMeta
    , metaFile :: FilePath
    -- ^Path to metadata file
    , intermediate :: Bool
    -- ^Whether to write intermediate frames
    , firstPush :: Bool
    }

instance OutputBackend PDBWriter where
    getNextPush PDBWriter {..}
        | intermediate = PushDump $ \dump _ step frame score -> pushPDBStep dump step frame score
        | otherwise = if firstPush
            then PushDump $ \_ _ _ _ _ -> modify $ \s -> s { firstPush = False }
            else PushMove $ \_ _ _ _ -> pure ()
    closeBackend PDBWriter {..} = do
        hClose pdbHandle
        withFile metaFile WriteMode $ \h -> writePDBMeta h meta
    pushLastFrame dump step frame score = do
        intermediate <- gets intermediate
        unless intermediate $ pushPDBStep dump step frame score

openPDBOutput ::
    OutputSettings
    -> Dump
    -- ^Current state of the simulation
    -> Bool
    -- ^Use simple PDB names?
    -> Bool
    -- ^Output intermediate steps?
    -> IO PDBWriter
    -- ^Opened PDB backend
openPDBOutput OutputSettings{..} dump simplePDB intermediate = do
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
    let firstPush = True
    pure PDBWriter{..}
  where
    pdbError = "The PDB format can't handle this number of different beads, binders or chains."

-- |Append a step to the output file
pushPDBStep :: (Show score) => Dump -> StepCounter -> FrameCounter -> score -> StateT PDBWriter IO ()
pushPDBStep dump' step frame score = do
    PDBWriter {..} <- get
    let frameHeader = StepHeader { headerSeqNum = frame
                                 , headerStep = step
                                 , headerTitle = "chromosome;bonds=" ++ show score
                                 }
    liftIO $ writePDB pdbHandle frameHeader meta dump >> hPutStrLn pdbHandle "END"
    when firstPush $ modify $ \s -> s { firstPush = False }
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
