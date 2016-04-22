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
    { sRepr :: !repr
    , sScore :: !score
    , sPreCallbackResults :: ![CallbackResult 'Pre]
    , sPostCallbackResults :: ![CallbackResult 'Post]
    , sStepCounter :: !Int
    }

-- |Describes how the simulation should run.
data RunSettings repr score backend = RunSettings
    { numSteps :: Int
    -- ^ Number of simulation steps.
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
    , outputBackend :: backend
    }

type SimT repr score = StateT (SimulationState repr score)
type RandomRepr m repr = (Generates (Double ': ReprRandomTypes m repr) m, Representation m repr)

step :: (RandomRepr m repr, Score score) => SimT repr score m (Maybe Move)
step = runMaybeT $ do
    st@SimulationState{..} <- get
    move <- lift2 (generateMove sRepr) >>= maybe mzero pure
    sScore' <- lift2 $ updateCallback sRepr sScore move

    let delta = fromIntegral $ sScore' - sScore
    unless (delta >= 0) $ do
        r <- lift2 $ getRandomR (0, 1)
        guard $ r < exp (delta * factor)

    put <=< lift2 $ do
        sPreCallbackResults' <- mapM (updateCallbackResult sRepr move) sPreCallbackResults
        (sRepr', _) <- performMove move sRepr
        sPostCallbackResults' <- mapM (updateCallbackResult sRepr' move) sPostCallbackResults
        pure $ st { sRepr = sRepr'
                  , sScore = sScore'
                  , sPreCallbackResults = sPreCallbackResults'
                  , sPostCallbackResults = sPostCallbackResults'
                  }
    pure move
  where
    factor :: Double
    factor = 2

    lift2 = lift . lift
{-# INLINE step #-}

stepAndWrite :: (MonadRandom m, RandomRepr m repr, Score score, MonadIO m, OutputBackend backend)
    => Handle -> backend -> Bool -> SimT repr score m ()
stepAndWrite callbacksHandle backend verbose = do
    move' <- step
    SimulationState{..} <- get
    case move' of
      Just move -> do
        writeCallbacks callbacksHandle verbose
        push <- liftIO $ getNextPush backend
        case push of
            PushDump f -> getDump >>= liftIO . (\d -> f d sStepCounter sScore)
            PushMove f -> liftIO $ f move
            DoNothing -> pure ()
      Nothing -> pure ()
    modify $ \s -> s { sStepCounter = sStepCounter + 1 }

  where
    getDump = gets sRepr >>= fmap removeLamins . lift . makeDump
    removeLamins d = d { dumpBinders = filter notLamin $ dumpBinders d }
    notLamin b = b ^. binderType /= laminType
{-# INLINE stepAndWrite #-}

writeCallbacks :: MonadIO m => Handle -> Bool -> SimT repr score m ()
writeCallbacks handle verbose = do
    preStr <- fmap resultStr <$> gets sPreCallbackResults
    postStr <- fmap resultStr <$> gets sPostCallbackResults
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

simulate :: (Score score, RandomRepr m repr, MonadIO m, OutputBackend backend)
    => RunSettings repr score backend -> Dump -> m Dump
simulate (RunSettings{..} :: RunSettings repr score backend) dump = do
    freezePredicate <- case freezeFile of
        Just file -> liftIO (parseFromFile freezePredicateParser file) >>= either (fail . show) pure
        Nothing -> pure freezeNothing
    repr :: repr <- loadDump dump freezePredicate

    let (enabledPreCallbacks, remainingCallbacks) = filterCallbacks allPreCallbacks requestedCallbacks
        (enabledPostCallbacks, remainingCallbacks') = filterCallbacks allPostCallbacks remainingCallbacks
    unless (null remainingCallbacks') . error $
        "Unrecognized callbacks: " ++ intercalate ", " remainingCallbacks'

    st <- do
        sScore :: score <- runCallback repr
        sPreCallbackResults <- getCallbackResults repr enabledPreCallbacks
        sPostCallbackResults <- getCallbackResults repr enabledPostCallbacks
        let sStepCounter = 0
            sRepr = repr
        pure SimulationState{..}

    let callbacksHandle = stdout
    st' <- flip execStateT st $ do
        replicateM_ numSteps $ stepAndWrite callbacksHandle outputBackend verboseCallbacks
        d <- gets sRepr >>= lift . makeDump
        stepC <- gets sStepCounter
        score' <- gets sScore
        liftIO $ pushLastFrame outputBackend d stepC score'
    makeDump $ sRepr st'
{-# INLINEABLE simulate #-}
