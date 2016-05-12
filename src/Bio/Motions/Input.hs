{- |
Module      : Bio.Motions.Input
Description : Typeclass for input formats
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Bio.Motions.Input
     where

import Bio.Motions.Types
import Bio.Motions.Utils.Random
import Bio.Motions.Representation.Class
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Bio.Motions.Callback.Class


data InputSettings = InputSettings
    { inputFile :: FilePath
    }

class (MonadIO m, ReadRepresentation m repr) => MoveProducer m repr p where
    getMove :: (Score score) => p -> repr -> score -> m (Maybe (Move, score))
    closeProducer :: p -> IO ()


data MoveGenerator = MoveGenerator
type RandomRepr m repr = (Generates (Double ': ReprRandomTypes m repr) m, Representation m repr)

instance (MonadIO m, RandomRepr m repr) => MoveProducer m repr MoveGenerator  where
    getMove _ repr score = runMaybeT $ do
        move <- lift (generateMove repr) >>= maybe mzero pure
        score' <- lift $ updateCallback repr score move

        let delta = fromIntegral $ score' - score
        unless (delta >= 0) $ do
            r <- lift $ getRandomR (0, 1)
            guard $ r < exp (delta * factor)
        return (move, score')
     where
       factor :: Double
       factor = 2
    closeProducer _ = pure ()
