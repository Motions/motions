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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#define CHECK_TYPES
module Bio.Motions.Callback.Dict.IO
    ( IODict
    ) where

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Dict
import Bio.Motions.Representation.Class
import Bio.Motions.Types
import Control.Monad.IO.Class
import qualified Data.HashTable.IO as H
import Data.Proxy
import Data.IORef
import Data.Typeable
import Unsafe.Coerce

data CallbackData mode m repr score cb where
    CallbackData :: Callback mode cb =>
        { cbValue :: !cb -- ^The value of this callback
        , cbIteration :: {-# UNPACK #-} !Int -- ^The iteration this callback was computed
        , cbRun :: !(IODict m repr score -> repr -> m cb) -- ^Specialised 'runCallback'
        , cbUpdate :: !(IODict m repr score -> repr -> cb -> Move -> m cb)
        } -> CallbackData mode m repr score cb

data CallbackWrapper mode m repr score where
    CallbackWrapper :: Typeable cb => !(CallbackData mode m repr score cb) -> CallbackWrapper mode m repr score

unwrapCallback :: forall mode m repr score cb. Typeable cb => CallbackWrapper mode m repr score -> Maybe (CallbackData mode m repr score cb)
unwrapCallback (CallbackWrapper (dat :: CallbackData mode m repr score cb'))
#ifdef CHECK_TYPES
    | typeRep (Proxy :: Proxy cb) == typeRep (Proxy :: Proxy cb') = Just $ unsafeCoerce dat
    | otherwise = Nothing
#else
    = Just $ unsafeCoerce dat
#endif
{-# INLINE unwrapCallback #-}

type Hashtable mode m repr score = H.BasicHashTable TypeRep (CallbackWrapper mode m repr score)

data IODict m repr score = IODict
    { postCallbacks :: {-# UNPACK #-} !(Hashtable 'Post m repr score)
    , curIteration :: {-# UNPACK #-} !(IORef Int)
    , curMove :: {-# UNPACK #-} !(IORef Move)
    , curScore :: {-# UNPACK #-} !(IORef score)
    }

fetchData :: forall cb m mode repr score. (Typeable cb, MonadIO m) => Hashtable mode m repr score -> m (Maybe (CallbackData mode m repr score cb))
fetchData hashtable = fmap (unwrapCallback =<<) . liftIO $  H.lookup hashtable (typeRep (Proxy :: Proxy cb))
{-# INLINE fetchData #-}

saveData :: forall cb mode m repr score. (Typeable cb, MonadIO m) => Hashtable mode m repr score -> CallbackData mode m repr score cb -> m ()
saveData hashtable = liftIO . H.insert hashtable (typeRep (Proxy :: Proxy cb)) . CallbackWrapper
{-# INLINE saveData #-}

instance (MonadIO m, CallbackRepresentation m repr, Score score) => CallbackCache m repr score (IODict m repr score) where
    getPostCallback dict@IODict{..} repr = do
        iter <- liftIO $ readIORef curIteration
        fetchData postCallbacks >>= \case
            Just cb@CallbackData{..} -> if
                | cbIteration == iter -> pure cbValue
                | cbIteration + 1 == iter -> do
                    move <- liftIO $ readIORef curMove
                    cbValue' <- cbUpdate dict repr cbValue move
                    saveData postCallbacks cb{cbValue = cbValue', cbIteration = iter}
                    pure cbValue'
                | otherwise -> do
                    cbValue' <- cbRun dict repr
                    saveData postCallbacks cb{cbValue = cbValue', cbIteration = iter}
                    pure cbValue'
            Nothing -> do
                cbValue <- runCallback dict repr
                saveData postCallbacks CallbackData
                    { cbValue = cbValue
                    , cbIteration = iter
                    , cbRun = runCallback
                    , cbUpdate = updateCallback
                    }
                pure cbValue
    {-# INLINE getPostCallback #-}

    getScore = liftIO . readIORef . curScore
    {-# INLINE getScore #-}

instance (MonadIO m, CallbackRepresentation m repr, Score score) => CallbackDict m repr score (IODict m repr score) where
    dictPerformMove IODict{..} !move !score = liftIO $ do
        modifyIORef' curIteration (+1)
        writeIORef curMove move 
        writeIORef curScore score
    {-# INLINE dictPerformMove #-}
