{- |
Module      : Bio.Motions.Callback.Dict.IO
Description : Contains an aggresively-caching, IO-based callback dictionary
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Bio.Motions.Callback.Dict.IO
    ( IODict
    ) where

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Dict
import Bio.Motions.Types
import qualified Data.HashTable.IO as H
import Data.Proxy
import Data.Typeable

data CallbackData mode m repr cb where
    CallbackData :: Callback mode cb =>
        { cbValue :: !cb -- ^The value of this callback
        , cbIteration :: {-# UNPACK #-} !Int -- ^The iteration this callback was computed
        , cbRun :: !(IODict m repr -> repr -> m cb) -- ^Specialised 'runCallback'
        , cbUpdate :: !(IODict m repr -> repr -> cb -> Move -> m b)
        } -> CallbackData mode m repr cb

data CallbackWrapper mode m repr where
    CallbackWrapper :: {-# UNPACK #-} !(CallbackData mode m repr cb) -> CallbackWrapper mode m repr

type Hashtable mode m repr = H.BasicHashTable TypeRep (CallbackWrapper mode m repr)

data IODict m repr score = IODict
    { postCallbacks :: {-# UNPACK #-} !(Hashtable 'Post m repr)
    , curIteration :: {-# UNPACK #-} !(IORef Int)
    , curMove :: {-# UNPACK #-} !(IORef Move)
    , curScore :: {-# UNPACK #-} !(IORef score)
    }

fetchData :: forall cb m mode repr. MonadIO m => Hashtable mode m repr -> m (Maybe (CallbackData mode m repr cb))
fetchData hashtable = liftIO $ (cast =<<) <$> H.lookup (typeRep (Proxy :: Proxy cb)) hashtable
{-# INLINE fetchData #-}

saveData :: forall cb mode m repr. Hashtable mode m repr -> CallbackData mode m repr cb -> m ()
saveData hashtable = insert hashtable (typeRef (Proxy :: Proxy cb))
{-# INLINE saveData #-}

instance (MonadIO m, ReadRepresentation m repr) => CallbackCache m repr (IODict m repr) where
    getPostCallback dict@IODict{..} repr = do
        iter <- liftIO $ readIORef
        fetchData postCallbacks >>= \case
            Just cb@CallbackData{..} -> if
                | cbIteration == iter -> pure cbValue
                | cbIteration + 1 == iter -> do
                    move <- liftIO $ readIORef curMove
                    cbValue' <- cbUpdate dict repr cbValue move
                    saveData dict cb{cbValue = cbValue', cbIteration = iter}
                | otherwise -> do
                    cbValue' <- cbRun dict repr
                    saveData dict cb{cbValue = cbValue', cbIteration = iter}
            Nothing -> do
                cbValue <- runCallback dict repr
                saveData dict CallbackData
                    { cbValue = cbValue
                    , cbIteration = iter
                    , cbRun = runCallback
                    , cbUpdate = updateCallback
                    }
    {-# INLINE getPostCallback #-}

    getScore = liftIO . readIORef . curScore
    {-# INLINE getScore #-}

instance (MonadIO m, ReadRepresentation m repr) => CallbackDict m repr (IODict m repr) where
    dictPerformMove IODict{..} !move !score = liftIO $ do
        modifyIORef' curIteration (+1)
        writeIORef curMove move 
        writeIORef curScore score
    {-# INLINE dictPerformMove #-}
