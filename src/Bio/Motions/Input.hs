{- |
Module      : Bio.Motions.Input
Description : Typeclass for input formats
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Bio.Motions.Input
    ( MoveGenerator
    , mkMoveGenerator
    , InputSettings (..)
    , ProdMove (..)
    , RandomRepr
    , MoveProducer
    , getMove
    ) where

import Bio.Motions.Types
import Bio.Motions.Utils.Random
import Bio.Motions.Representation.Class
import Bio.Motions.Callback.Class

import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import GHC.Generics
import Data.Int


data InputSettings = InputSettings
    { inputFiles :: [FilePath]
    , metaFile :: Maybe FilePath
    , binaryInput :: Bool
    , moveSource :: String
    , skipFrames :: Int
    } deriving Generic

data ProdMove s = Stop | MakeMove Move s StepCounter

-- |Produces moves.
class ReadRepresentation m repr => MoveProducer m repr prod where
    -- |Produce a new move. The move could be generated or read from some other source (e.g. a file).
    -- If a move is successfuly produced, then the current score and step counter is also returned.
    -- If no moves are available, Stop is returned.
    -- May throw IO errors.
    getMove :: Score score => repr -> score -> StateT prod m (ProdMove score)

-- |Generates random moves for the actual simulation
data MoveGenerator = MoveGenerator
    { stepCounter :: StepCounter
    , numSteps :: Int64
    }

mkMoveGenerator :: Int64 -> MoveGenerator
mkMoveGenerator numSteps = let stepCounter = 0 in MoveGenerator {..}

type RandomRepr m repr = (Generates (Double ': ReprRandomTypes m repr) m, Representation m repr)

instance (MonadIO m, RandomRepr m repr) => MoveProducer m repr MoveGenerator where
    getMove repr score = go
      where
        go = get >>= \p -> if stepCounter p > numSteps p
            then pure Stop
            else do
                put (p { stepCounter = stepCounter p + 1 })
                lift generate >>= maybe go wrap

        generate = runMaybeT $ do
            move <- lift (generateMove repr) >>= maybe mzero pure
            score' <- lift $ updateCallback repr score move

            let delta = fromIntegral $ score' - score
            unless (delta >= 0) $ do
                r <- lift $ getRandomR (0, 1)
                guard $ r < exp (delta * factor)
            pure (move, score')

        wrap (m, s) = MakeMove m s <$> gets stepCounter
        {-# INLINE wrap #-}

        factor :: Double
        factor = 2
    {-# INLINABLE getMove #-}
