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
import Data.IORef
import System.IO

data PDBWriter = PDBWriter
    { pdbHandle :: Handle
    , meta :: PDBMeta
    , metaFile :: FilePath
    -- ^Path to metadata file
    , intermediate :: Bool
    -- ^Whether to write intermediate frames
    , firstPush :: IORef Bool
    -- ^When intermediate == false, getNextPush will always return PushMove, except the first push,
    --  which has to return PushDump. This Bool indicates the first push.
    }

instance OutputBackend PDBWriter where
    getNextPush writer@PDBWriter {..}
        | intermediate = pure $ PushDump $ \dump _ step frame score -> pushPDBStep dump step frame score writer
        | otherwise = do
            firstPush' <- readIORef firstPush
            pure $ if firstPush'
                then PushDump $ \_ _ _ _ _ -> writeIORef firstPush False
                else PushMove $ \_ _ _ _ -> pure ()
    closeBackend PDBWriter {..} = do
        hClose pdbHandle
        withFile metaFile WriteMode $ \h -> writePDBMeta h meta
    pushLastFrame dump step frame score writer@PDBWriter {..}
        | intermediate = pure ()
        | otherwise = pushPDBStep dump step frame score writer

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
    firstPush <- newIORef True
    pure PDBWriter{..}
  where
    pdbError = "The PDB format can't handle this number of different beads, binders or chains."

-- |Append a step to the output file
pushPDBStep :: (Show score) => Dump -> StepCounter -> FrameCounter -> score -> PDBWriter -> IO ()
pushPDBStep dump' step frame score PDBWriter {..} = do
    let frameHeader = StepHeader { headerSeqNum = frame
                                 , headerStep = step
                                 , headerTitle = "chromosome;bonds=" ++ show score
                                 }
    liftIO $ writePDB pdbHandle frameHeader meta dump >> hPutStrLn pdbHandle "END"
    modifyIORef firstPush $ const False
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
