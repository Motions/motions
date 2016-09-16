{- |
Module      : Bio.Motions.PDB.Backend.Reader
Description : MoveProducer instance for PDB
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
module Bio.Motions.PDB.Backend.Reader where

import Bio.Motions.Input
import Bio.Motions.Types
import Bio.Motions.PDB.Read
import Bio.Motions.PDB.Internal(RevPDBMeta,mergeDumps,ReadError)
import Bio.Motions.PDB.Meta
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class

import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import System.IO
import System.IO.Error
import Control.Exception(bracket)

data PDBInFile = PDBInFile
    { handle :: Handle
    , end :: Bool
    , lastDump :: Dump
    , lastStep :: Maybe StepCounter
    }

data PDBReader = PDBReader
    { files :: [PDBInFile]
    , revMeta :: RevPDBMeta
    , maxChainDistSquared :: Int
    }

instance (MonadIO m, ReadRepresentation m repr) => MoveProducer m repr PDBReader where
    getMove repr score = do
        dump <- lift $ makeDump repr
        getMove' dump >>= \case
            Nothing -> pure Stop
            Just (move, step) -> do
                score' <- lift $ updateCallback repr score move
                pure $ MakeMove move score' step
    {-# INLINEABLE getMove #-}

openPDBInput :: InputSettings -> Int -> IO PDBReader
openPDBInput InputSettings{..} maxChainDistSquared = do
    mf <- maybe (fail "Specify an input meta file") pure metaFile
    revMeta <- eitherFail "Meta file read error: " $ withFile mf ReadMode readPDBMeta
    files <- mapM (openSingle revMeta) inputFiles
    pure PDBReader{..}
  where
    openSingle :: RevPDBMeta -> FilePath -> IO PDBInFile
    openSingle revMeta path = do
        handle <- openFile path ReadMode
        frame <- readPDBData handle
        let end = False
        (lastDump, lastStep) <- case parseFrame frame >>= fromPDBData revMeta (Just maxChainDistSquared) of
          Left e -> fail e
          Right d -> pure d
        pure PDBInFile{..}

withPDBInput :: InputSettings
             -> Int
             -- ^Max chain distance
             -> (PDBReader -> [String] -> [String] -> IO a)
             -- ^reader -> chain names -> binder types names -> a
             -> IO a
withPDBInput s dist f = bracket (openPDBInput s dist) close (\r -> f r (chainNames r) (binderTypesNames r))
  where
    close :: PDBReader -> IO ()
    close PDBReader{..} = mapM_ (hClose . handle) files
    chainNames = getChainNames . revMeta
    binderTypesNames = getBinderTypesNames . revMeta

getMove' :: MonadIO m => Dump -> StateT PDBReader m (Maybe (Move, StepCounter))
getMove' dump = eitherFail "PDB read error: " . runExceptT $
    getFrame >>= mapM (\(move, step) -> fmap (, step) . either diffError pure . diffDumps dump $ move)
  where
    diffError e = throwError $ "adjacent frames don't match: " ++ e
{-# INLINEABLE getMove' #-}

-- |Read the next frame from input files, or Nothing if *all* files have reached EOF
-- Otherwise, files that have ended are frozen at their last frame and merged with
-- data from files that have more frames.
-- Frames from the first file must contain step numbers in its headers.
getFrame :: MonadIO m => ExceptT ReadError (StateT PDBReader m) (Maybe (Dump, StepCounter))
getFrame = get >>= \reader@PDBReader {..} ->
    if all end files
        then pure Nothing
        else fmap Just $ do
            files' <- mapM (advanceSingleFile revMeta maxChainDistSquared) files
            put reader { files = files' }
            stepCounter <- maybe noStepError pure . lastStep . head $ files
            fmap (, stepCounter) $ liftEither $ mergeDumps $ map lastDump files
  where
    advanceSingleFile :: (MonadIO m, MonadError ReadError m) => RevPDBMeta -> Int -> PDBInFile -> m PDBInFile
    advanceSingleFile revMeta maxChainDistSquared file@PDBInFile{..} = do
        ((dump', step'), end') <- readCatch handle >>= \case
            Nothing -> pure ((lastDump, lastStep), True)
            Just frame -> (, False) <$> parse frame
        pure file { lastDump = dump', lastStep = step', end = end'}
      where
        parse :: (MonadError ReadError m) => [BS.ByteString] -> m (Dump, Maybe StepCounter)
        parse x = liftEither $ parseFrame x >>= fromPDBData revMeta (Just maxChainDistSquared)
        {-# INLINEABLE parse #-}
    {-# INLINEABLE advanceSingleFile #-}

    readCatch :: MonadIO m => Handle -> m (Maybe [BS.ByteString])
    readCatch hdl = liftIO $ catchIOError (Just <$> readPDBData hdl)
          (\e -> if isEOFError e then pure Nothing else ioError e)
    {-# INLINEABLE readCatch #-}

    noStepError = throwError "a frame in the first PDB file does not have the step counter"

{-# INLINEABLE getFrame #-}

skipPDBInput :: Int -> StateT PDBReader IO (Dump, StepCounter)
skipPDBInput n = do
    replicateM_ n readFrame
    readFrame >>= maybe (fail "unexpected end of file") pure
  where
    readFrame = eitherFail "PDB read error: " . runExceptT $ getFrame

eitherFail :: (MonadIO m) => String -> m (Either String a) -> m a
eitherFail errorPrefix m = m >>= either (fail . (errorPrefix ++)) pure

liftEither :: (MonadError e m) => Either e a -> m a
liftEither = either throwError pure
