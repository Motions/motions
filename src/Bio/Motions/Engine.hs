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
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Bio.Motions.Engine where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class
import Bio.Motions.Output
import Bio.Motions.PDB.Backend
import Bio.Motions.Format.Handle
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.FreezePredicateParser
import Text.Parsec.String

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import System.IO
import Control.Lens
import Data.List
import Data.Maybe

data SimulationState repr score = SimulationState
    { repr :: repr
    , score :: score
    , preCallbackResults :: [CallbackResult Pre]
    , postCallbackResults :: [CallbackResult Post]
    , stepCounter :: Int
    {-, frameCounter :: Int-}
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
    , requestedCallbacksFile :: FilePath
    -- ^ List of requested callback names
    }

step :: (MonadRandom m, MonadState (SimulationState repr score) m,
         Representation (MaybeT m) repr, Score score) => m (Maybe Move)
step = runMaybeT $ do
    st@SimulationState{..} <- get
    move <- generateMove repr
    newScore <- updateCallback repr score move
    newPreCallbackResults <- mapM (updateCallbackResult repr move) preCallbackResults

    let delta = fromIntegral $ newScore - score
    unless (delta >= 0) $ do
        r <- getRandomR (0, 1)
        guard $ r < exp (delta * factor)

    (newRepr, _) <- performMove move repr
    newPostCallbackResults <- mapM (updateCallbackResult newRepr move) postCallbackResults

    put st { repr = newRepr
           , score = newScore
           , preCallbackResults = newPreCallbackResults
           , postCallbackResults = newPostCallbackResults
           }

    pure move
  where
    factor :: Double
    factor = 2


stepAndWrite :: _ => Handle -> b -> Bool -> m ()
stepAndWrite callbacksHandle backend verbose = do
    oldScore <- gets score
    (Just move) <- step -- TODO: kiedy to jest Nothing?
    newScore <- gets score

    when (oldScore /= newScore) $ do
        {-put st { frameCounter = frameCounter + 1 }-}
        writeCallbacks callbacksHandle verbose
        push <- liftIO $ getNextPush backend
        case push of
            {-PushDump f -> pushPDBStep handle pdbMeta-}
            PushDump f -> getDump >>= liftIO . f
            PushMove f -> liftIO $ f move
            DoNothing -> pure ()
    modify $ \s -> s { stepCounter = stepCounter s + 1 }

    where getDump = gets repr >>= \x -> removeLamins <$> makeDump x
          removeLamins d = d { dumpBinders = filter notLamin $ dumpBinders d }
          notLamin b = b ^. binderType /= laminType


writeCallbacks :: _ => Handle -> Bool -> m ()
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

simulate :: _ => RunSettings repr score -> Dump -> m Dump
simulate (RunSettings{..} :: RunSettings repr score) dump = do
    freezePredicate <- case freezeFile of
        Just file -> liftIO (parseFromFile freezePredicateParser file) >>= either (fail . show) pure
        Nothing -> pure freezeNothing
    repr :: repr <- loadDump dump freezePredicate

    let evs = nub . map dumpBeadEV . concat . dumpChains $ dump
        bts = nub . map (^. binderType) . dumpBinders $ dump
        chs = nub . map (^. beadChain) . concat . dumpIndexedChains $ dump
        {-mkMeta = if simplePDB then mkSimplePDBMeta else mkPDBMeta-}
        {-pdbMeta = fromMaybe (error pdbError) $ mkMeta evs bts chs-}

    requestedCallbacks <- liftIO $ lines <$> readFile requestedCallbacksFile
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
    st' <- flip execStateT st $ do
        {-pushPDBLamins pdbLaminHandle pdbMeta-}
        {-when writeIntermediatePDB $ pushPDBStep pdbHandle pdbMeta-}
        {-replicateM_ numSteps $ stepAndWrite callbacksHandle-}
            {-(guard writeIntermediatePDB >> Just pdbHandle) verboseCallbacks pdbMeta-}
        {-[>unless writeIntermediatePDB $ pushPDBStep pdbHandle pdbMeta<]-}
        return ()

    let SimulationState{..} = st'
    makeDump repr
  where
    pdbError = "The PDB format can't handle this number of different beads, binders or chains."
