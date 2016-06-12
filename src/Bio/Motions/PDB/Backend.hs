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
import Control.Monad.Except
import Data.List
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.IORef
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
    { handle :: Handle
    , end :: Bool
    , lastDump :: Dump
    }

data PDBReader = PDBReader
    { files :: [IORef PDBInFile]
    , revMeta :: RevPDBMeta
    , maxChainDistSquared :: Int
    }

openPDBInput :: InputSettings -> Int -> IO PDBReader
openPDBInput InputSettings{..} maxChainDistSquared = do
    let mf = fromMaybe (error "Specify an input meta file") metaFile
    revMeta <- eitherFail "Meta file read error: " $ withFile mf ReadMode readPDBMeta
    files <- mapM (openSingle revMeta) inputFiles
    return PDBReader{..}
  where
    openSingle :: RevPDBMeta -> FilePath -> IO (IORef PDBInFile)
    openSingle revMeta path = do
        handle <- openFile path ReadMode
        frame <- readPDBData handle
        let end = False
            lastDump = case parseFrame frame >>= fromPDBData revMeta (Just maxChainDistSquared) of
                         Left e -> error e
                         Right d -> d
        newIORef PDBInFile{..}

withPDBInput :: InputSettings -> Int -> (PDBReader -> [String] -> [String]-> IO a) -> IO a
withPDBInput s dist f = bracket (openPDBInput s dist) close (\r -> f r (chainNames r) (binderTypesNames r))
  where
    close :: PDBReader -> IO ()
    close PDBReader{..} = mapM_ (readIORef >=> hClose . handle) files
    chainNames = getChainNames . revMeta
    binderTypesNames = getBinderTypesNames . revMeta

instance (MonadIO m, ReadRepresentation m repr) => MoveProducer m repr PDBReader where
    getMove reader repr score = do
        dump <- makeDump repr
        getMove' reader dump >>= \case
            Nothing -> return Stop
            Just move -> do
                score' <- updateCallback repr score move
                return $ MakeMove move score'

getMove' :: MonadIO m => PDBReader -> Dump -> m (Maybe Move)
getMove' reader dump = eitherFail "PDB read error: " . runExceptT $
    getFrame reader >>= mapM (either diffError return . diffDumps dump)
  where
    diffError e = throwError $ "adjacent frames don't match: " ++ e

getFrame :: (MonadIO m, MonadError ReadError m) => PDBReader -> m (Maybe Dump)
getFrame PDBReader{..} = do
    isEnd <- liftIO $ all end <$> mapM readIORef files
    if isEnd then return Nothing
         else fmap Just $ do
             newDumps <- liftIO $ mapM (fmap lastDump . readIORef) files
             forM_ files $ \x -> liftIO (readIORef x) >>= advanceSingleFile >>= liftIO . writeIORef x
             liftEither $ mergeDumps newDumps
  where
      advanceSingleFile :: (MonadIO m, MonadError ReadError m) => PDBInFile -> m PDBInFile
      advanceSingleFile file@PDBInFile{..} = do
          (dump', end') <- readCatch handle >>= \case
                            Nothing -> return (lastDump, True)
                            Just frame -> parse frame >>= \d -> return (d, False)
          return file{lastDump = dump', end = end'}
      readCatch :: MonadIO m => Handle -> m (Maybe [BS.ByteString])
      readCatch hdl = liftIO $ catchIOError (Just <$> readPDBData hdl)
                    (\e -> if isEOFError e then return Nothing else ioError e)

      parse :: (MonadError ReadError m) => [BS.ByteString] -> m Dump
      parse x = liftEither $ parseFrame x >>= fromPDBData revMeta (Just maxChainDistSquared)

skipPDBInput :: PDBReader -> Int -> IO Dump
skipPDBInput reader 0 =
    fromMaybe (error "unexpected end of file") <$> (eitherFail "PDB read error: " . runExceptT $ getFrame reader)
skipPDBInput reader@PDBReader{..} n = do
    _ <- eitherFail "PDB read error: " . runExceptT $ getFrame reader
    skipPDBInput reader (n - 1)

eitherFail :: (MonadIO m) => String -> m (Either String a) -> m a
eitherFail errorPrefix m = m >>= either (fail . (errorPrefix ++)) pure

liftEither :: (MonadError e m) => Either e a -> m a
liftEither = either throwError return
