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
import Bio.Motions.PDB.Write
import Bio.Motions.PDB.Meta
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Chain
import Bio.Motions.Callback.StandardScore
import Bio.Motions.EnabledCallbacks

import Control.Applicative
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Trans.Maybe
import System.IO
import Control.Lens
import Data.Proxy

data SimulationState repr score = SimulationState
    { repr :: repr
    , score :: score
    , preCallbackResults :: [CallbackResult Pre]
    , postCallbackResults :: [CallbackResult Post]
    , stepCounter :: Int
    , frameCounter :: Int
    , writeNextFrame :: Bool
    }

-- |Describes how the simulation should run.
data RunSettings repr score = RunSettings
    { pdbFile :: FilePath
    -- ^ Path to the PDB output file.
    , numSteps :: Int
    -- ^ Number of simulation steps.
    , writeIntermediatePDB :: Bool
    -- ^ Whether to write intermediate PDB frames.
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

pushPDB :: _ => Handle -> m ()
pushPDB handle = do
    st@SimulationState{..} <- get
    dump <- makeDump repr

    let frameHeader = FrameHeader { headerSeqNum = frameCounter
                                  , headerStep = stepCounter
                                  , headerTitle = "chromosome;bonds=" ++ show score
                                  }

    liftIO $ writePDB handle frameHeader legacyPDBMeta dump >> hPutStrLn handle "END"

    put st { frameCounter = frameCounter + 1 }

writeAndStep :: _ => Handle -> Maybe Handle -> m ()
writeAndStep callbacksHandle pdbHandle = do
    write callbacksHandle pdbHandle

    oldScore <- gets score
    step -- TODO: do something with the move
    newScore <- gets score

    modify $ \s -> s { stepCounter = stepCounter s + 1
                     , writeNextFrame = oldScore /= newScore
                     }

write :: _ => Handle -> Maybe Handle -> m ()
write callbacksHandle pdbHandle = do
    writeFrame <- gets writeNextFrame
    when writeFrame $ do
        map writeCallbackResult <$> gets preCallbackResults
        map writeCallbackResult <$> gets postCallbackResults
        maybe (pure ()) pushPDB pdbHandle
  where
    writeCallbackResult (CallbackResult cb) =
        hPutStrLn callbacksHandle $ getCallbackName cb ++ ": " ++ show cb

simulate :: _ => RunSettings repr score -> Dump -> m Dump
simulate (RunSettings{..} :: RunSettings repr score) dump = do
    repr :: repr <- loadDump dump
    score :: score <- runCallback repr

    preCallbackResults <- getCallbackResults repr enabledPreCallbacks
    postCallbackResults <- getCallbackResults repr enabledPostCallbacks
    let stepCounter = 0
        frameCounter = 0
        writeNextFrame = True
        st = SimulationState{..}

    let callbacksHandle = stdout
    pdbHandle <- liftIO $ guard writeIntermediatePDB >> Just <$> openFile pdbFile WriteMode
    st' <- flip execStateT st $ replicateM_ numSteps (writeAndStep callbacksHandle pdbHandle)
                                >> write callbacksHandle pdbHandle
    liftIO $ maybe (pure ()) hClose pdbHandle
    let SimulationState{..} = st'

    makeDump repr
