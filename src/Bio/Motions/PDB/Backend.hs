{- |
Module      : Bio.Motions.PDB.Backend
Description : OutputBackend instance for PDB output
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bio.Motions.PDB.Backend where

import Bio.Motions.Common
import Bio.Motions.Output
import Bio.Motions.Input
import Bio.Motions.Types
import Bio.Motions.PDB.Write
import Bio.Motions.PDB.Read
import Bio.Motions.PDB.Internal(RevPDBMeta)
import Bio.Motions.PDB.Meta
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Class
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
        | otherwise = pure . PushMove $ \_ cb _ -> writeCallbacks cbHandle cbVerbose cb
    closeBackend PDBBackend{..} = do
        hClose pdbHandle
        withFile metaFile WriteMode $ \h -> writePDBMeta h meta
    pushLastFrame backend dump step score
        | intermediate backend = pure ()
        | otherwise = pushPDBStep backend dump ([], []) step score

-- |Open PDB output
openPDBOutput ::
       OutputSettings
    -> Dump
    -- ^Dump of the first frame
    -> Bool
    -- ^Use simplePDB
    -> Bool
    -- ^Write intermediate frames
    -> Handle
    -- ^Callback output handle
    -> Bool
    -- ^Verbose callbacks
    -> IO PDBBackend
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
pushPDBStep :: (Show score) => PDBBackend -> Dump -> Callbacks -> StepCounter -> score -> IO ()
pushPDBStep PDBBackend{..} dump' callbacks step score = do
    modifyIORef frameCounter (+1)
    frame <- readIORef frameCounter
    let frameHeader = StepHeader { headerSeqNum = frame
                                 , headerStep = step
                                 , headerTitle = "chromosome;bonds=" ++ show score
                                 }
    liftIO $ writePDB pdbHandle frameHeader meta dump >> hPutStrLn pdbHandle "END"
    liftIO $ writeCallbacks cbHandle cbVerbose callbacks
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

data PDBReader = PDBReader
    { handles :: [Handle]
    , revMeta :: RevPDBMeta
    }

openPDBInput :: InputSettings -> IO (PDBReader, Dump)
openPDBInput InputSettings{..} = do
    handles <- mapM (`openFile` ReadMode) inputFiles
    let mf = fromMaybe (error "Specify an input meta file") metaFile
    revMeta <- either (fail . ("Meta file read error: " ++)) pure =<<
        withFile mf ReadMode readPDBMeta
    dump <- either (fail . ("PDB read error: " ++)) pure =<< readPDB handles revMeta
    return (PDBReader{..}, dump)

instance (MonadIO m, ReadRepresentation m repr) => MoveProducer m repr PDBReader where
    getMove PDBReader{..} repr score = do
        dump <- makeDump repr
        dump' <- liftIO $ readPDB handles revMeta >>= either (fail . ("PDB read error: " ++)) pure
        let move = either (error "diff error") id $ diffDumps dump dump'
        score' <- updateCallback repr score move
        return $ Just (move, score')
    closeProducer PDBReader{..} = mapM_ hClose handles
