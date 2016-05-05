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
import Bio.Motions.Callback.Class

import Control.Lens
import System.IO
import Data.IORef
import Control.Monad.State
import Data.List
import Data.Maybe

data PDBBackend = PDBBackend
    { pdbHandle :: Handle
    , frameCounter :: IORef Int
    -- ^Number of frames written
    , meta :: PDBMeta
    , metaFile :: FilePath
    -- ^Path to metadata file
    , intermediate :: Bool
    -- ^Whether to write intermediate frames
    , cbHandle :: Handle
    -- ^Callback output handle
    , cbVerbose :: Bool
    -- ^Verbose callbacks
    }

instance OutputBackend PDBBackend where
    getNextPush st@PDBBackend{..}
        | intermediate = pure $ PushDump (pushPDBStep st)
        | otherwise = pure . PushMove $ \_ cb -> writeCallbacks cbHandle cbVerbose cb
    closeBackend PDBBackend{..} = do
        hClose pdbHandle
        withFile metaFile WriteMode $ \h -> writePDBMeta h meta
    pushLastFrame backend dump step score
        | intermediate backend = pure ()
        | otherwise = pushPDBStep backend dump ([], []) step score

openPDBOutput :: OutputSettings -> Dump -> Bool -> Bool -> Handle -> Bool -> IO PDBBackend
openPDBOutput OutputSettings{..} dump simplePDB intermediate cbHandle cbVerbose = do
    let pdbFile = outputPrefix ++ ".pdb"
        laminFile = outputPrefix ++ "-lamin.pdb"
        metaFile = pdbFile ++ ".meta"
    let evs = nub . map dumpBeadEV . concat . dumpChains $ dump
        bts = nub . map (^. binderType) . dumpBinders $ dump
        chs = nub . map (^. beadChain) . concat . dumpIndexedChains $ dump
        mkMeta = if simplePDB then mkSimplePDBMeta else mkPDBMeta
        meta = fromMaybe (error pdbError) $ mkMeta evs bts chs
    pdbHandle <- openFile pdbFile WriteMode
    withFile laminFile WriteMode $ \h -> pushPDBLamins h meta dump
    frameCounter <- newIORef 0
    return PDBBackend{..}
  where
    pdbError = "The PDB format can't handle this number of different beads, binders or chains."

-- |Append a step to the output file
pushPDBStep :: (Show score) => PDBBackend -> Dump -> Callbacks -> Int -> score -> IO ()
pushPDBStep PDBBackend{..} dump' callbacks step score = do
    frame <- readIORef frameCounter
    let frameHeader = StepHeader { headerSeqNum = frame
                                 , headerStep = step
                                 , headerTitle = "chromosome;bonds=" ++ show score
                                 }
    liftIO $ writePDB pdbHandle frameHeader meta dump >> hPutStrLn pdbHandle "END"
    liftIO $ writeCallbacks cbHandle cbVerbose callbacks
    modifyIORef frameCounter (+1)
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

-- |Output callbacks in text form
writeCallbacks :: MonadIO m => Handle -> Bool -> Callbacks -> m ()
writeCallbacks handle verbose (preCbs, postCbs) = do
    let preStr = resultStr <$> preCbs
        postStr = resultStr <$> postCbs
    liftIO . hPutStrLn handle . intercalate separator $ preStr ++ postStr
  where
    --TODO?
    resultStr (CallbackResult cb) = (if verbose then getCallbackName cb ++ ": " else "") ++ show cb
    separator = if verbose then "\n" else " "
