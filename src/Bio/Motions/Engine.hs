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
import Bio.Motions.Output
import Bio.Motions.PDB.Backend
import Bio.Motions.Format.Handle
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

data SimulationState repr score = SimulationState
    { repr :: !repr
    , score :: !score
    , preCallbackResults :: ![CallbackResult 'Pre]
    , postCallbackResults :: ![CallbackResult 'Post]
    , stepCounter :: !Int
    {-, frameCounter :: Int-}
    }

-- |Describes how the simulation should run.
data RunSettings repr score = RunSettings
    { outputPrefix :: FilePath
    -- ^ Path to the PDB output file.
    , numSteps :: Int
    -- ^ Number of simulation steps.
    , binaryOutput :: Bool
    -- ^ Use binary format for output
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
type RandomRepr m repr = (Generates (Double ': ReprRandomTypes m repr) m, Representation m repr)

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

stepAndWrite :: (MonadRandom m, RandomRepr m repr, Score score, MonadIO m, OutputBackend b)
    => Handle -> b -> Bool -> SimT repr score m ()
stepAndWrite callbacksHandle backend verbose = do
    move' <- step
    case move' of
      Just move -> do
        writeCallbacks callbacksHandle verbose
        push <- liftIO $ getNextPush backend
        case push of
            {-PushDump f -> pushPDBStep handle pdbMeta-}
            PushDump f -> getDump >>= liftIO . f
            PushMove f -> liftIO $ f move
            DoNothing -> pure ()
      Nothing -> pure ()
    modify $ \s -> s { stepCounter = stepCounter s + 1 }

    where getDump = gets repr >>= \x -> removeLamins <$> lift ( makeDump x)
          removeLamins d = d { dumpBinders = filter notLamin $ dumpBinders d }
          notLamin b = b ^. binderType /= laminType
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


    let (enabledPreCallbacks, remainingCallbacks) = filterCallbacks allPreCallbacks requestedCallbacks
        (enabledPostCallbacks, remainingCallbacks') = filterCallbacks allPostCallbacks remainingCallbacks
    unless (null remainingCallbacks') . error $
        "Unrecognized callbacks: " ++ intercalate ", " remainingCallbacks'

    score :: score <- runCallback repr
    preCallbackResults <- getCallbackResults repr enabledPreCallbacks
    postCallbackResults <- getCallbackResults repr enabledPostCallbacks
    let stepCounter = 0
        st = SimulationState{..}
        simulationName = ""
        simulationDescription = ""
        chainNames = []
        outSettings = OutputSettings{..}
        fpkf = 1000  -- TODO
    let pdb = liftIO $ openPDBOutput outSettings dump writeIntermediatePDB simplePDB
        bin = liftIO $ openBinaryOutput fpkf outSettings dump
        sim :: (OutputBackend b, MonadIO m, MonadRandom m, RandomRepr m repr) => b -> m Dump
        sim = sim' st
    if binaryOutput then
                    bin >>= sim
                    else
                    pdb >>= sim
    where sim' st backend = do
            let callbacksHandle = stdout
            st' <- flip execStateT st $ do
                replicateM_ numSteps $ stepAndWrite callbacksHandle backend verboseCallbacks
                d <- gets (\SimulationState{repr = r} -> r) >>= lift . makeDump
                liftIO $ forceFullPush backend d
            liftIO $ bClose backend
            let SimulationState{..} = st'
            makeDump repr
{-# INLINEABLE simulate #-}
