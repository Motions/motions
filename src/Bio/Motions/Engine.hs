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
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Bio.Motions.Engine where

import Bio.Motions.Types
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class
import Bio.Motions.Output
import Bio.Motions.Input
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Data.List
import System.IO

data SimulationState repr score backend producer = SimulationState
    { repr :: !repr
    , score :: !score
    , preCallbackResults :: ![CallbackResult 'Pre]
    , postCallbackResults :: ![CallbackResult 'Post]
    , stepCounter :: !StepCounter
    , frameCounter :: !FrameCounter
    , outputBackend :: !backend
    , moveProducer :: !producer
    }

-- |Describes how the simulation should run.
data RunSettings repr score backend producer = RunSettings
    { freezePredicate :: FreezePredicate
    -- ^ A predicate determining whether a bead is frozen
    , allPreCallbacks :: [CallbackType 'Pre]
    -- ^ List of all available pre-callbacks' types
    , allPostCallbacks :: [CallbackType 'Post]
    -- ^ List of all available post-callbacks' types
    , requestedCallbacks :: [String]
    -- ^ List of requested callback names
    , backend :: backend
    -- ^ Output backend
    , producer :: producer
    -- ^ Move producer
    , verboseCallbacks :: Bool
    -- ^ Whether callbacks should have their names written in the output
    , loggingHandle :: Maybe Handle
    -- ^ Where the run log should be written or Nothing if logging is disabled.
    }

type SimT repr score backend producer m =
    ExceptT () (StateT (SimulationState repr score backend producer) m)

step :: (RandomRepr m repr, Score score, MoveProducer m repr producer) =>
        SimT repr score backend producer m Move
step = do
    st@SimulationState{..} <- get
    (prodMove, producer') <- lift2 $ flip runStateT moveProducer $ getMove repr score
    case prodMove of
        MakeMove move score' stepCounter' -> do
            put <=< lift2 $ do
                preCallbackResults' <- mapM (updateCallbackResult repr move) preCallbackResults
                repr' <- performMove move repr
                postCallbackResults' <- mapM (updateCallbackResult repr' move) postCallbackResults
                pure $ st { repr = repr'
                          , score = score'
                          , preCallbackResults = preCallbackResults'
                          , postCallbackResults = postCallbackResults'
                          , stepCounter = stepCounter'
                          , moveProducer = producer'
                          }
            pure move
        Stop -> throwError ()
  where
    lift2 = lift . lift
{-# INLINE step #-}

-- |Perform a step and output the results.
stepAndWrite :: (RandomRepr m repr, Score score, MonadIO m,
                    OutputBackend backend, MoveProducer m repr producer) =>
      (SimulationState repr score backend producer -> IO ())
    -- ^A logging function.
   -> SimT repr score backend producer m ()
stepAndWrite log = do
    move <- step
    modify $ \s -> s { frameCounter = frameCounter s + 1 }
    cb <- (,) <$> gets preCallbackResults <*> gets postCallbackResults
    st@SimulationState{..} <- get
    outputBackend' <- case getNextPush outputBackend of
      PushDump act -> do
          dump <- lift . lift . makeDump $ repr
          liftIO . flip execStateT outputBackend $ act dump cb stepCounter frameCounter score
      PushMove act -> liftIO . flip execStateT outputBackend $ act move cb stepCounter frameCounter
    let st' = st { outputBackend = outputBackend' }
    put st'
    liftIO $ log st'
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

simulate :: (Score score, RandomRepr m repr, MonadIO m, OutputBackend backend, MoveProducer m repr producer)
         => RunSettings repr score backend producer -> StepCounter -> Dump -> m Dump
simulate (RunSettings{..} :: RunSettings repr score backend producer) initialStep dump = do
    st <- initState
    SimulationState{..} <- flip execStateT st . runExceptT $ do
        -- push first frame
        cb <- (,) <$> gets preCallbackResults <*> gets postCallbackResults
        backend <- gets outputBackend
        stepCounter <- gets stepCounter
        backend' <- case getNextPush backend of
            PushDump act -> liftIO  . flip execStateT backend $ act dump cb stepCounter 0 (score st)
            PushMove _ -> error "first frame must always be a dump"
        modify $ \st -> st { outputBackend = backend' }
        forever $ stepAndWrite log

    finalDump <- makeDump repr
    liftIO . flip evalStateT outputBackend $ pushLastFrame finalDump stepCounter frameCounter score
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

        let stepCounter = initialStep
            frameCounter = 0
            outputBackend = backend
            moveProducer = producer
        pure SimulationState{..}
    {-# INLINE initState #-}

    log st = forM_ loggingHandle $ \h -> writeLog h verboseCallbacks st
    {-# INLINE log #-}
{-# INLINEABLE[0] simulate #-}

-- |Output log: step and frame counters, score, and callback results
writeLog :: (MonadIO m, Show score) => Handle -> Bool -> SimulationState repr score backend producer -> m ()
writeLog handle verbose SimulationState{..} = liftIO . hPutStrLn handle $ logStr
  where
    logStr = unwords $ [stepStr, frameStr, scoreStr] ++ preCbsStr ++ postCbsStr
    stepStr = "iter: " ++ show stepCounter
    frameStr = "frame: " ++ show frameCounter
    scoreStr = "energy: " ++ show score
    preCbsStr = callbackStr <$> preCallbackResults
    postCbsStr = callbackStr <$> postCallbackResults
    callbackStr (CallbackResult cb) = (if verbose then getCallbackName cb ++ ": " else "") ++ show cb
