{- |
Module      : Bio.Motions.PDB.Backend
Description : OutputBackend instance for PDB output
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
module Bio.Motions.PDB.Backend where

import Bio.Motions.Common
import Bio.Motions.Output
import Bio.Motions.Input
import Bio.Motions.Types
import Bio.Motions.PDB.Write
import Bio.Motions.PDB.Read
import Bio.Motions.PDB.Internal(RevPDBMeta,mergeDumps,ReadError)
import Bio.Motions.PDB.Meta
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class

import Control.Lens
import Control.Monad.State
import Data.List
import Data.Maybe
import System.IO
import System.IO.Error
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

data PDBInFile = PDBInFile 
    { _handle :: Handle
    , _end :: Bool
    , _lastDump :: Maybe Dump
    }

data PDBReader = PDBReader
    { _files :: [PDBInFile]
    , _revMeta :: RevPDBMeta
    , _maxChainDistSquared :: Int
    }
makeLenses ''PDBInFile
makeLenses ''PDBReader


openPDBInput :: InputSettings -> Int -> IO PDBReader
openPDBInput InputSettings{..} _maxChainDistSquared = do
    _files <- mapM openSingle inputFiles
    let mf = fromMaybe (error "Specify an input meta file") metaFile
    _revMeta <- eitherFail "Meta file read error: " $ withFile mf ReadMode readPDBMeta
    return PDBReader{..}
  where
    openSingle :: FilePath -> IO PDBInFile
    openSingle = (`openFile` ReadMode) >>= undefined
    {-openSingle = (`openFile` ReadMode) >>= (\h -> return $ PDBInFile h False Nothing)-}

withPDBInput :: InputSettings -> Int -> (PDBReader -> [String] -> [String]-> IO a) -> IO a
withPDBInput s dist f = bracket (openPDBInput s dist) close (\r -> f r (chainNames r) (binderTypesNames r))
  where
    close :: PDBReader -> IO ()
    close PDBReader{..} = mapM_ (hClose . _handle) _files
    chainNames = getChainNames . _revMeta
    binderTypesNames = getBinderTypesNames . _revMeta

instance (MonadIO m, ReadRepresentation m repr) => MoveProducer m repr PDBReader where
    getMove reader repr score = do
        dump <- makeDump repr
        evalStateT (getMoveS dump) reader >>= \case
            Nothing -> return Stop
            Just move -> do
                score' <- updateCallback repr score move
                return $ MakeMove move score'


getMoveS :: (MonadIO m, MonadState PDBReader m) => Dump -> m (Maybe Move)
getMoveS = undefined
        {-eof <- liftIO $ or <$> mapM hIsEOF handles-}
        {-if eof then return Stop-}
               {-else do-}
                   {-dump <- makeDump repr-}
                   {-dump' <- liftIO $ eitherFail "PDB read error: " $-}
                                {-readPDB handles revMeta $ Just maxChainDistSquared-}
                   {-let move = either (\e -> error $ "adjacent frames don't match: " ++ e ) id $-}
                                    {-diffDumps dump dump'-}
                   {-score' <- updateCallback repr score move-}
                   {-return $ MakeMove move score'-}

getFrameS :: (MonadIO m, MonadState PDBReader m) => m (Maybe Dump)
getFrameS = do
    isEnd <- gets (all _end . _files)
    if isEnd then return Nothing
     else do
         -- todo lol types
         aaa <- eitherFail "PDB read error: " $ do
             {-hdls <- use files-}
             zoom (files.traversed) getSingleFile
             return undefined
             {-return $ mergeDumps dumps-}
         return $ Just aaa
  where
      getSingleFile :: (MonadIO m, MonadState PDBInFile m) => m (Either ReadError Dump)
      getSingleFile = undefined
      readCatch hdl = catchIOError (Just <$> readPDBData hdl)
                    (\e -> if isEOFError e then return Nothing else ioError e)

skipPDBInput :: PDBReader -> Int -> IO Dump
skipPDBInput = undefined
{-skipPDBInput PDBReader{..} 0 =-}
    {-eitherFail "PDB read error: " $ readPDB handles revMeta $ Just maxChainDistSquared-}
{-skipPDBInput reader@PDBReader{..} n = do-}
    {-_ <- eitherFail "PDB read error: " $ readPDB handles revMeta $ Just maxChainDistSquared-}
    {-skipPDBInput reader (n - 1)-}

eitherFail :: (MonadIO m) => String -> m (Either String a) -> m a
eitherFail errorPrefix m = m >>= either (fail . (errorPrefix ++)) pure
