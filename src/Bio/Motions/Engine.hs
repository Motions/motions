{- |
Module      : Bio.Motions.Engine
Description : Contains the simulation engine.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module Bio.Motions.Engine where

import Bio.Motions.Types
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class
import Control.Applicative
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Trans.Maybe

-- |Contains the simulatiom state: a 'Representation' 'repr' and a 'Score' 'score'.
data SimulationState repr score = forall s. SimulationState
    { repr :: repr s
    , score :: score
    }

-- |A simple implementation (just to be sure it typechecks). TODO: rewrite.
-- As a dirty hack, it requires the 'score' to be 'Integral'.
simulateStep :: (Monad m, Alternative m, Representation m repr, Integral score,
    MonadRandom m, Score m score, MonadState (SimulationState repr score) m) => m (Wrap Move)
simulateStep = do
    SimulationState{..} <- get
    move <- generateMove repr
    newScore <- updateCallback repr score move

    let delta = fromIntegral $ newScore - score
    unless (delta >= 0) $ do
        r <- getRandomR (0, 1)
        guard $ r < exp (delta * factor)

    (newRepr, _) <- performMove move repr
    put SimulationState { repr = newRepr, score = newScore }

    pure $ Wrap move
  where
    factor :: Double
    factor = 2
