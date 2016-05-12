{- |
Module      : Bio.Motions.Input
Description : Typeclass for input formats
License     : Apache
Stability   : experimental
Portability : unportable
-}
module Bio.Motions.Input where

import Bio.Motions.Types
import Bio.Motions.Utils.Random
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Class
{-import Bio.Motions.Callback.Class-}
import Control.Monad.IO.Class


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

class MoveProducer p where
    getMove :: (MonadIO m, MonadRandom m, ReadRepresentation m repr) => p -> repr -> m (Maybe Move)


data MoveGenerator

instance MoveProducer MoveGenerator where
    getMove _ = generateMove
