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
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Bio.Motions.Engine where

import Bio.Motions.Types
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class
import Bio.Motions.Output
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Random

import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import Data.List
import System.IO

data SimulationState repr score = SimulationState
    { repr :: !repr
    , score :: !score
    , preCallbackResults :: ![CallbackResult 'Pre]
    , postCallbackResults :: ![CallbackResult 'Post]
    , stepCounter :: !StepCounter
    , frameCounter :: !FrameCounter
    }

-- |Describes how the simulation should run.
data RunSettings repr score backend = RunSettings
    { numSteps :: Int
    -- ^ Number of simulation steps.
    , freezePredicate :: FreezePredicate
    -- ^ A predicate determining whether a bead is frozen
    , allPreCallbacks :: [CallbackType 'Pre]
    -- ^ List of all available pre-callbacks' types
    , allPostCallbacks :: [CallbackType 'Post]
    -- ^ List of all available post-callbacks' types
    , requestedCallbacks :: [String]
    -- ^ List of requested callback names
    , outputBackend :: backend
    -- ^ Output backend
    , verboseCallbacks :: Bool
    -- ^ Whether callbacks should have their names written in the output
    , loggingHandle :: Maybe Handle
    -- ^ Where the run log should be written or Nothing if logging is disabled.
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

    put <=< lift2 $ do
        preCallbackResults' <- mapM (updateCallbackResult repr move) preCallbackResults
        repr' <- performMove move repr
        postCallbackResults' <- mapM (updateCallbackResult repr' move) postCallbackResults
        pure $ st { repr = repr'
                  , score = score'
                  , preCallbackResults = preCallbackResults'
                  , postCallbackResults = postCallbackResults'
                  }
    pure move
  where
    factor :: Double
    factor = 2

    lift2 = lift . lift
{-# INLINE step #-}

-- |Perform a step and output the results.
stepAndWrite :: (MonadRandom m, RandomRepr m repr, Score score, MonadIO m, OutputBackend backend) =>
       backend
    -- ^The output backend.
   -> (SimulationState repr score -> IO ())
    -- ^A logging function.
   -> SimT repr score m ()
stepAndWrite backend log = do
    modify $ \s -> s { stepCounter = stepCounter s + 1 }
    step >>= \case
      Nothing -> pure ()
      Just move -> do
          modify $ \s -> s { frameCounter = frameCounter s + 1 }
          cb <- (,) <$> gets preCallbackResults <*> gets postCallbackResults
          st@SimulationState{..} <- get
          liftIO (getNextPush backend) >>= \case
            PushDump act -> getDump >>= liftIO . (\dump -> act dump cb stepCounter frameCounter score)
            PushMove act -> liftIO $ act move cb stepCounter frameCounter
          liftIO $ log st
  where
    getDump = gets repr >>= lift . makeDump
{-# INLINE stepAndWrite #-}

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
    st <- initState

    SimulationState{..} <- flip execStateT st $
        replicateM_ numSteps $ stepAndWrite outputBackend log

    finalDump <- makeDump repr
    liftIO $ pushLastFrame outputBackend finalDump stepCounter frameCounter score
    pure finalDump
  where
    initState = do
        repr :: repr <- loadDump dump freezePredicate
        score :: score <- runCallback repr

        let (enabledPreCallbacks, remainingCallbacks) = filterCallbacks allPreCallbacks requestedCallbacks
            (enabledPostCallbacks, remainingCallbacks') = filterCallbacks allPostCallbacks remainingCallbacks
        unless (null remainingCallbacks') . error $
            "Unrecognized callbacks: " ++ intercalate ", " remainingCallbacks'
        preCallbackResults <- getCallbackResults repr enabledPreCallbacks
        postCallbackResults <- getCallbackResults repr enabledPostCallbacks

        let stepCounter = 0
            frameCounter = 0
        pure SimulationState{..}
    {-# INLINE initState #-}

    log st = forM_ loggingHandle $ \h -> writeLog h verboseCallbacks st
    {-# INLINE log #-}
{-# INLINEABLE[0] simulate #-}

-- |Output log: step and frame counters, score, and callback results
writeLog :: (MonadIO m, Show score) => Handle -> Bool -> SimulationState repr score -> m ()
writeLog handle verbose SimulationState{..} = liftIO . hPutStrLn handle $ logStr
  where
    logStr = unwords $ [stepStr, frameStr, scoreStr] ++ preCbsStr ++ postCbsStr
    stepStr = "iter: " ++ show stepCounter
    frameStr = "frame: " ++ show frameCounter
    scoreStr = "energy: " ++ show score
    preCbsStr = callbackStr <$> preCallbackResults
    postCbsStr = callbackStr <$> postCallbackResults
    callbackStr (CallbackResult cb) = (if verbose then getCallbackName cb ++ ": " else "") ++ show cb
