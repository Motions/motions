{- |
Module      : Bio.Motions.Engine
Description : Contains the simulation engine.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Bio.Motions.Engine where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class
import Bio.Motions.PDB.Write
import Bio.Motions.PDB.Meta
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.FreezePredicateParser
import Bio.Motions.Utils.Random
import Text.Parsec.String

import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import System.IO
import Control.Lens
import Data.List
import Data.Maybe

data SimulationState repr score = SimulationState
    { repr :: !repr
    , score :: !score
    , preCallbackResults :: ![CallbackResult 'Pre]
    , postCallbackResults :: ![CallbackResult 'Post]
    , stepCounter :: !Int
    , frameCounter :: !Int
    }

-- |Describes how the simulation should run.
data RunSettings repr score = RunSettings
    { pdbFile :: FilePath
    -- ^ Path to the PDB output file.
    , numSteps :: Int
    -- ^ Number of simulation steps.
    , writeIntermediatePDB :: Bool
    -- ^ Whether to write intermediate PDB frames.
    , verboseCallbacks :: Bool
    -- ^ Enable verbose callback output.
    , simplePDB :: Bool
    -- ^ Whether to write simpler residue/atom names in the PDB file.
    , freezeFile :: Maybe FilePath
    -- ^ A file containing the ranges of the frozen beads' indices.
    , allPreCallbacks :: [CallbackType 'Pre]
    -- ^ List of all available pre-callbacks' types
    , allPostCallbacks :: [CallbackType 'Post]
    -- ^ List of all available post-callbacks' types
    , requestedCallbacks :: [String]
    -- ^ List of requested callback names
    }

type SimT repr score = StateT (SimulationState repr score)
type RandomRepr m repr = (MonadRandomForAll (Double ': ReprRandomTypes m repr) m, Representation m repr)

step :: (RandomRepr m repr, Score score) => SimT repr score m (Maybe Move)
step = runMaybeT $ do
    st@SimulationState{..} <- get
    move <- lift2 (generateMove repr) >>= maybe mzero pure
    score' <- lift2 $ updateCallback repr score move

    let delta = fromIntegral $ score' - score
    unless (delta >= 0) $ do
        r <- lift2 $ getRandomR (0, 1)
        guard $ r < exp (delta * factor)

    st' <- lift2 $ do
        preCallbackResults' <- mapM (updateCallbackResult repr move) preCallbackResults
        (repr', _) <- performMove move repr
        postCallbackResults' <- mapM (updateCallbackResult repr' move) postCallbackResults
        pure $ st { repr = repr'
                  , score = score'
                  , preCallbackResults = preCallbackResults'
                  , postCallbackResults = postCallbackResults'
                  }
    put st'
    pure move
  where
    factor :: Double
    factor = 2

    lift2 = lift . lift
{-# INLINE step #-}

pushPDBStep :: (Show score, MonadIO m, Representation m repr) => Handle -> PDBMeta -> SimT repr score m ()
pushPDBStep handle pdbMeta = do
    st@SimulationState{..} <- get
    dump <- removeLamins <$> lift (makeDump repr)
    let frameHeader = StepHeader { headerSeqNum = frameCounter
                                 , headerStep = stepCounter
                                 , headerTitle = "chromosome;bonds=" ++ show score
                                 }
    liftIO $ writePDB handle frameHeader pdbMeta dump >> hPutStrLn handle "END"
    put st { frameCounter = frameCounter + 1 }
  where
    removeLamins d = d { dumpBinders = filter notLamin $ dumpBinders d }
    notLamin b = b ^. binderType /= laminType

pushPDBLamins :: (MonadIO m, Representation m repr) => Handle -> PDBMeta -> SimT repr score m ()
pushPDBLamins handle pdbMeta = do
    SimulationState{..} <- get
    dump <- filterLamins <$> lift (makeDump repr)
    liftIO $ writePDB handle LaminHeader pdbMeta dump >> hPutStrLn handle "END"
  where
    filterLamins d = Dump { dumpBinders = filter isLamin $ dumpBinders d, dumpChains = [] }
    isLamin b = b ^. binderType == laminType

stepAndWrite :: (RandomRepr m repr, Score score, MonadIO m)
    => Handle -> Maybe Handle -> Bool -> PDBMeta -> SimT repr score m ()
stepAndWrite callbacksHandle pdbHandle verbose pdbMeta = do
    move' <- step
    case move' of
      Just move -> do   --TODO will use this in another commit
        writeCallbacks callbacksHandle verbose
        case pdbHandle of
            Just handle -> pushPDBStep handle pdbMeta
            Nothing -> pure ()
      Nothing -> pure ()

    modify $ \s -> s { stepCounter = stepCounter s + 1 }
{-# INLINE stepAndWrite #-}

writeCallbacks :: MonadIO m => Handle -> Bool -> SimT repr score m ()
writeCallbacks handle verbose = do
    preStr <- fmap resultStr <$> gets preCallbackResults
    postStr <- fmap resultStr <$> gets postCallbackResults
    liftIO . hPutStrLn handle . intercalate separator $ preStr ++ postStr
  where
    resultStr (CallbackResult cb) = (if verbose then getCallbackName cb ++ ": " else "") ++ show cb
    separator = if verbose then "\n" else " "

-- |Parses a list of callback names
filterCallbacks ::
       [CallbackType mode]
    -- ^List of all available callback types
    -> [String]
    -- ^Requested callback names
    -> ([CallbackType mode], [String])
    -- ^(Requested callback types, leftover names)
filterCallbacks allCbs req = ((m M.!) <$> found, notFound)
  where
    m = M.fromList [(callbackName p, x) | x@(CallbackType p) <- allCbs]
    (found, notFound) = partition (`M.member` m) req

simulate :: (Score score, RandomRepr m repr, MonadIO m)
    => RunSettings repr score -> Dump -> m Dump
simulate (RunSettings{..} :: RunSettings repr score) dump = do
    freezePredicate <- case freezeFile of
        Just file -> liftIO (parseFromFile freezePredicateParser file) >>= either (fail . show) pure
        Nothing -> pure freezeNothing
    repr :: repr <- loadDump dump freezePredicate

    let evs = nub . map dumpBeadEV . concat . dumpChains $ dump
        bts = nub . map (^. binderType) . dumpBinders $ dump
        chs = nub . map (^. beadChain) . concat . dumpIndexedChains $ dump
        mkMeta = if simplePDB then mkSimplePDBMeta else mkPDBMeta
        pdbMeta = fromMaybe (error pdbError) $ mkMeta evs bts chs
        pdbMetaFile = pdbFile ++ ".meta"
        pdbLaminFile = pdbFile ++ ".lamin"

    let (enabledPreCallbacks, remainingCallbacks) = filterCallbacks allPreCallbacks requestedCallbacks
        (enabledPostCallbacks, remainingCallbacks') = filterCallbacks allPostCallbacks remainingCallbacks
    unless (null remainingCallbacks') . error $
        "Unrecognized callbacks: " ++ intercalate ", " remainingCallbacks'

    score :: score <- runCallback repr
    preCallbackResults <- getCallbackResults repr enabledPreCallbacks
    postCallbackResults <- getCallbackResults repr enabledPostCallbacks
    let stepCounter = 0
        frameCounter = 0
        st = SimulationState{..}

    let callbacksHandle = stdout
    pdbHandle <- liftIO $ openFile pdbFile WriteMode
    pdbLaminHandle <- liftIO $ openFile pdbLaminFile WriteMode
    st' <- flip execStateT st $ do
        pushPDBLamins pdbLaminHandle pdbMeta
        when writeIntermediatePDB $ pushPDBStep pdbHandle pdbMeta
        replicateM_ numSteps $ stepAndWrite callbacksHandle
            (guard writeIntermediatePDB >> Just pdbHandle) verboseCallbacks pdbMeta
        unless writeIntermediatePDB $ pushPDBStep pdbHandle pdbMeta
    liftIO $ hClose pdbHandle
    liftIO $ hClose pdbLaminHandle
    liftIO $ withFile pdbMetaFile WriteMode $ \h -> writePDBMeta h pdbMeta

    let SimulationState{..} = st'
    makeDump repr
  where
    pdbError = "The PDB format can't handle this number of different beads, binders or chains."
{-# INLINEABLE simulate #-}
