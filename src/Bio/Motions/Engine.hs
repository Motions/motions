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
import Bio.Motions.Input
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Random

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.List

data SimulationState repr score = SimulationState
    { repr :: !repr
    , score :: !score
    , preCallbackResults :: ![CallbackResult 'Pre]
    , postCallbackResults :: ![CallbackResult 'Post]
    , stepCounter :: !Int
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
    }

type SimT repr score = StateT (SimulationState repr score)
{-type RandomRepr m repr = (Generates (Double ': ReprRandomTypes m repr) m, Representation m repr)-}

step :: (RandomRepr m repr, Score score, MoveProducer m repr p, MonadIO m) => p -> SimT repr score m (Maybe Move)
step producer = do
    st@SimulationState{..} <- get
    move' <- lift $ getMove producer repr score
    case move' of
      Nothing -> pure Nothing
      Just (move, score') -> do
        put <=< lift $ do
            preCallbackResults' <- mapM (updateCallbackResult repr move) preCallbackResults
            repr' <- performMove move repr
            postCallbackResults' <- mapM (updateCallbackResult repr' move) postCallbackResults
            pure $ st { repr = repr'
                      , score = score'
                      , preCallbackResults = preCallbackResults'
                      , postCallbackResults = postCallbackResults'
                      }
        pure $ Just move
{-# INLINE step #-}

stepAndWrite :: (MonadRandom m, RandomRepr m repr, Score score, MonadIO m, OutputBackend backend, MoveProducer m repr p)
             => p -> backend -> SimT repr score m ()
stepAndWrite producer backend = do
    step producer >>= \case
      Nothing -> pure ()
      Just move -> do
          cb <- (,) <$> gets preCallbackResults <*> gets postCallbackResults
          SimulationState{..} <- get
          liftIO (getNextPush backend) >>= \case
            PushDump act -> getDump >>= liftIO . (\dump -> act dump cb stepCounter score)
            PushMove act -> liftIO $ act move cb
    modify $ \s -> s { stepCounter = stepCounter s + 1 }
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
    let producer = MoveGenerator

    SimulationState{..} <- flip execStateT st $
        replicateM_ numSteps $ stepAndWrite producer outputBackend

    finalDump <- makeDump repr
    liftIO $ pushLastFrame outputBackend finalDump stepCounter score
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
        pure SimulationState{..}
    {-# INLINE initState #-}
{-# INLINEABLE[0] simulate #-}
