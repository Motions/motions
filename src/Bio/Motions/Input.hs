{- |
Module      : Bio.Motions.Input
Description : Typeclass for input formats
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Bio.Motions.Input
    (MoveProducer(..)
    ,MoveGenerator(..)
    ,InputBackend(..) --todo
    ,InputSettings(..)
    ) where

import Bio.Motions.Types
import Bio.Motions.Utils.Random
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Class
{-import Bio.Motions.Callback.Class-}
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State.Strict
import Bio.Motions.Callback.Class


data InputSettings = InputSettings
    { inputFile :: FilePath
    }

-- todo remove
class InputBackend a where
    openInput :: InputSettings -> IO a
    seekInput :: a -> Int -> IO ()
    tellInput :: a -> IO Int
    -- TODO nierobialne/bezużyteczne
    inputLength :: a -> IO Int
    -- TODO callbacks needed there?
    -- | Get a dump at curent position. If not possible directly, also returns
    -- a list of moves to apply. Advances iteration by one.
    getDump :: a -> IO (Dump, [Move])
    -- |Get a move at current position and advance by one.
    {-getMove :: a -> IO (Move, Callbacks)-}
    closeInput :: a -> IO ()

class MoveProducer m p where
    type ProdRandomTypes m repr :: [*]
    getMove :: (MonadIO m, ReadRepresentation m repr,
                Generates (ProdRandomTypes m repr) m,
                Score score) => p -> repr -> score -> m (Maybe (Move, score))


data MoveGenerator repr = MoveGenerator
type RandomRepr m repr = (Generates (Double ': ReprRandomTypes m repr) m, Representation m repr)

instance (RandomRepr m repr) => MoveProducer m (MoveGenerator repr) where
    type ProdRandomTypes m repr = ReprRandomTypes m repr
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
