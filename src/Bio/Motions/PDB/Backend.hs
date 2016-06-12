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
import Control.Monad.State
import Data.List
import Data.Maybe
import System.IO
import Control.Exception(bracket)

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
    -> Bool
    -- ^Use simple PDB names?
    -> Bool
    -- ^Output intermediate steps?
    -> IO PDBBackend
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
        $ error ("Fatal error: the number of binder type names differs from the number of binder types" ++ show binderTypesNames ++ show bts)
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

data PDBReader = PDBReader
    { handles :: [Handle]
    , revMeta :: RevPDBMeta
    , maxChainDistSquared :: Int
    }

openPDBInput :: InputSettings -> Int -> IO PDBReader
openPDBInput InputSettings{..} maxChainDistSquared = do
    handles <- mapM (`openFile` ReadMode) inputFiles
    let mf = fromMaybe (error "Specify an input meta file") metaFile
    revMeta <- eitherFail "Meta file read error: " $ withFile mf ReadMode readPDBMeta
    return PDBReader{..}

withPDBInput :: InputSettings -> Int -> (PDBReader -> [String] -> [String]-> IO a) -> IO a
withPDBInput s dist f = bracket (openPDBInput s dist) close (\r -> f r (chainN r) (binderTN r))
  where close PDBReader{..} = mapM_ hClose handles
        chainN = getChainNames . revMeta
        binderTN = getBinderTypesNames . revMeta

instance (MonadIO m, ReadRepresentation m repr) => MoveProducer m repr PDBReader where
    getMove PDBReader{..} repr score = do
        eof <- liftIO $ or <$> mapM hIsEOF handles
        if eof then return Stop
               else do
                   dump <- makeDump repr
                   dump' <- liftIO $ eitherFail "PDB read error: " $
                                readPDB handles revMeta $ Just maxChainDistSquared
                   let move = either (error "diff error") id $ diffDumps dump dump'
                   score' <- updateCallback repr score move
                   return $ MakeMove move score'

skipPDBInput :: PDBReader -> Int -> IO Dump
skipPDBInput PDBReader{..} 0 =
    eitherFail "PDB read error: " $ readPDB handles revMeta $ Just maxChainDistSquared
skipPDBInput reader@PDBReader{..} n =  do
    _ <- eitherFail "PDB read error: " $ readPDB handles revMeta $ Just maxChainDistSquared
    skipPDBInput reader (n - 1)

eitherFail :: String -> IO (Either String a) -> IO a
eitherFail errorPrefix m = m >>= either (fail . (errorPrefix ++)) pure
