{- |
Module      : Bio.Motions.Engine
Description : Contains the simulation engine.
License     : MIT
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
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class
import Control.Applicative
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Trans.Maybe

import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Chain
import Bio.Motions.Callback.StandardScore
import Bio.Motions.EnabledCallbacks
import System.IO
import Control.Lens
import Bio.Motions.PDB.Write
import Bio.Motions.PDB.Meta

data SimulationState repr score = SimulationState
    { repr :: repr
    , score :: score
    , preCallbackResults :: [CallbackWrapper Pre]
    , postCallbackResults :: [CallbackWrapper Post]
    , stepCounter :: Int
    , frameCounter :: Int
    }

-- |Describes how the simulation should run.
data SimulationSettings = SimulationSettings
    { pdbFile :: FilePath
    -- ^ Path to the PDB output file.
    , numSteps :: Int
    -- ^ Number of simulation steps.
    , writeIntermediatePDB :: Bool
    }

-- |A simple implementation (just to be sure it typechecks). TODO: rewrite.
-- As a dirty hack, it requires the 'score' to be 'Integral'.
step :: (MonadRandom m, MonadState (SimulationState repr score) m, Integral score,
         Representation (MaybeT m) repr, Score score) => m (Maybe Move)
step = runMaybeT $ do
    st@SimulationState{..} <- get
    move <- generateMove repr
    newScore <- updateCallback repr score move
    newPreCallbackResults <- mapM (updateCallbackWrapper repr move) preCallbackResults

    let delta = fromIntegral $ newScore - score
    unless (delta >= 0) $ do
        r <- getRandomR (0, 1)
        guard $ r < exp (delta * factor)

    (newRepr, _) <- performMove move repr
    newPostCallbackResults <- mapM (updateCallbackWrapper newRepr move) postCallbackResults

    put st { repr = newRepr
           , score = newScore
           , preCallbackResults = newPreCallbackResults
           , postCallbackResults = newPostCallbackResults
           }

    pure move
  where
    factor :: Double
    factor = 2

pushPDB :: _ => Maybe Handle -> m ()
pushPDB Nothing = pure ()
pushPDB (Just handle) = do
    st@SimulationState{..} <- get
    dump <- makeDump repr

    let frameHeader = FrameHeader { headerSeqNum = frameCounter
                                  , headerStep = stepCounter
                                  , headerTitle = "chromosome;bonds=" ++ show score
                                  }

    liftIO $ writePDB handle frameHeader legacyPDBMeta dump >> hPutStrLn handle "END"

    put st { frameCounter = frameCounter + 1 }

stepAndWrite :: _ => Bool -> Handle -> m ()
stepAndWrite writeIntermediatePDB handle = do
    oldScore <- gets score
    step -- TODO: do something with the move
    newScore <- gets score

    when (oldScore /= newScore) $ do
        map writeCallbackResult <$> gets preCallbackResults
        map writeCallbackResult <$> gets postCallbackResults
        pushPDB $ if writeIntermediatePDB then Just handle else Nothing

    modify $ \s -> s { stepCounter = stepCounter s + 1 }
  where
    writeCallbackResult (CallbackWrapper cb) = hPutStrLn handle $ getCallbackName cb ++ ": " ++ show cb

simulate :: (MonadIO m, MonadRandom m, Representation m PureChainRepresentation) =>
    SimulationSettings -> Dump -> m Dump
simulate SimulationSettings{..} dump = do
    repr :: PureChainRepresentation <- loadDump dump
    score :: StandardScore <- runCallback repr
    preCallbackResults <- getCallbackResults repr enabledPreCallbacks
    postCallbackResults <- getCallbackResults repr enabledPostCallbacks
    let stepCounter = 0
        frameCounter = 0
        st = SimulationState{..}
    st' <- flip execStateT st . replicateM_ numSteps $ stepAndWrite writeIntermediatePDB stdout
    let SimulationState{..} = st'
    makeDump repr
