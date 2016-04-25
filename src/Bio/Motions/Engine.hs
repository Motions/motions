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
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Bio.Motions.Engine where

import Bio.Motions.Types
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Specialise
import Bio.Motions.Output
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Random

import Data.Proxy

import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as M
import System.IO
import Data.List

data SimulationState repr score m = SimulationState
    { repr :: !repr
    , score :: !score
    , preCallbackResults :: ![SpecCallbackValue 'Pre m repr]
    , postCallbackResults :: ![SpecCallbackValue 'Post m repr]
    , stepCounter :: !Int
    }

-- |Describes how the simulation should run.
data RunSettings repr score backend (preCbs :: [*]) (postCbs :: [*]) = RunSettings
    { numSteps :: Int
    -- ^ Number of simulation steps.
    , verboseCallbacks :: Bool
    -- ^ Enable verbose callback output.
    , freezePredicate :: FreezePredicate
    -- ^ A predicate determining whether a bead is frozen
    , requestedCallbacks :: [String]
    -- ^ List of requested callback names
    , outputBackend :: backend
    -- ^ Output backend
    }

type SimT repr score m = StateT (SimulationState repr score m) m
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
        preCallbackResults' <- mapM (updateSpecCallback repr move) preCallbackResults
        repr' <- performMove move repr
        postCallbackResults' <- mapM (updateSpecCallback repr' move) postCallbackResults
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

stepAndWrite :: (MonadRandom m, RandomRepr m repr, Score score, MonadIO m, OutputBackend backend)
    => Handle -> backend -> Bool -> SimT repr score m ()
stepAndWrite callbacksHandle backend verbose = do
    step >>= \case
      Nothing -> pure ()
      Just move -> do
          writeCallbacks callbacksHandle verbose
          SimulationState{..} <- get
          liftIO (getNextPush backend) >>= \case
            PushDump act -> getDump >>= liftIO . (\dump -> act dump stepCounter score)
            PushMove act -> liftIO $ act move
            DoNothing -> pure ()
    modify $ \s -> s { stepCounter = stepCounter s + 1 }
  where
    getDump = gets repr >>= lift . makeDump
{-# INLINE stepAndWrite #-}

writeCallbacks :: MonadIO m => Handle -> Bool -> SimT repr score m ()
writeCallbacks handle verbose = do
    preStr <- fmap resultStr <$> gets preCallbackResults
    postStr <- fmap resultStr <$> gets postCallbackResults
    liftIO . hPutStrLn handle . intercalate separator $ preStr ++ postStr
  where
    resultStr (SpecCallbackValue _ cb) = (if verbose then getCallbackName cb ++ ": " else "") ++ show cb
    separator = if verbose then "\n" else " "

-- |Parses a list of callback names
filterCallbacks ::
       [SpecCallbackType mode m repr]
    -- ^List of all available callback types
    -> [String]
    -- ^Requested callback names
    -> ([SpecCallbackType mode m repr], [String])
    -- ^(Requested callback types, leftover names)
filterCallbacks allCbs req = ((m M.!) <$> found, notFound)
  where
    m = M.fromList [(callbackName p, x) | x@SpecCallback{scValue = p} <- allCbs]
    (found, notFound) = partition (`M.member` m) req

simulate :: (Score score, RandomRepr m repr, MonadIO m, OutputBackend backend, SpecCallbacks 'Pre preCbs, SpecCallbacks 'Post postCbs)
    => RunSettings repr score backend preCbs postCbs -> Dump -> m Dump
simulate (RunSettings{..} :: RunSettings repr score backend preCbs postCbs) dump = do
    let callbacksHandle = stdout
    st <- initState

    SimulationState{..} <- flip execStateT st $
         replicateM_ numSteps $ stepAndWrite callbacksHandle outputBackend verboseCallbacks

    finalDump <- makeDump repr
    liftIO $ pushLastFrame outputBackend finalDump stepCounter score
    pure finalDump
  where
    initState = do
        repr :: repr <- loadDump dump freezePredicate
        score :: score <- runCallback repr

        let allPreCallbacks = makeSpecCallbacks (Proxy :: Proxy preCbs)
            allPostCallbacks = makeSpecCallbacks (Proxy :: Proxy postCbs)
            (enabledPreCallbacks, remainingCallbacks) = filterCallbacks allPreCallbacks requestedCallbacks
            (enabledPostCallbacks, remainingCallbacks') = filterCallbacks allPostCallbacks remainingCallbacks
        unless (null remainingCallbacks') . error $
            "Unrecognized callbacks: " ++ intercalate ", " remainingCallbacks'
        preCallbackResults <- runSpecCallbacks repr enabledPreCallbacks
        postCallbackResults <- runSpecCallbacks repr enabledPostCallbacks

        let stepCounter = 0
        pure SimulationState{..}
    {-# INLINE initState #-}
{-# INLINEABLE[0] simulate #-}
