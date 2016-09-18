{- |
Module      : Bio.Motions.Utils.Ref
Description : Fast references
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RoleAnnotations #-}
module Bio.Motions.Utils.Ref
    ( URef()
    , newURef
    , readURef
    , writeURef
    , modifyURef
    , IOURef()
    , newIOURef
    , readIOURef
    , writeIOURef
    , modifyIOURef
    ) where

import Control.Monad.IO.Class
import Control.Monad.Primitive
import Data.Primitive

newtype URef s a = URef (MutableByteArray s)
type role URef nominal representational
newtype IOURef a = IOURef (URef (PrimState IO) a)

newURef :: (Prim a, PrimMonad m) => a -> m (URef (PrimState m) a)
newURef val = do
    arr <- newByteArray 1
    writeByteArray arr 1 val
    pure $ URef arr
{-# INLINE newURef #-}

readURef :: (Prim a, PrimMonad m) => URef (PrimState m) a -> m a
readURef (URef arr) = readByteArray arr 1
{-# INLINE readURef #-}

writeURef :: (Prim a, PrimMonad m) => URef (PrimState m) a -> a -> m ()
writeURef (URef arr) = writeByteArray arr 1
{-# INLINE writeURef #-}

modifyURef :: (Prim a, PrimMonad m) => URef (PrimState m) a -> (a -> a) -> m ()
modifyURef ref f = readURef ref >>= writeURef ref . f
{-# INLINE modifyURef #-}

newIOURef :: (Prim a, MonadIO m) => a -> m (IOURef a)
newIOURef a = liftIO $ IOURef <$> newURef a
{-# INLINE newIOURef #-}

readIOURef :: (Prim a, MonadIO m) => IOURef a -> m a
readIOURef (IOURef ref) = liftIO $ readURef ref
{-# INLINE readIOURef #-}

writeIOURef :: (Prim a, MonadIO m) => IOURef a -> a -> m ()
writeIOURef (IOURef ref) = liftIO . writeURef ref
{-# INLINE writeIOURef #-}

modifyIOURef :: (Prim a, MonadIO m) => IOURef a -> (a -> a) -> m ()
modifyIOURef (IOURef ref) = liftIO . modifyURef ref
{-# INLINE modifyIOURef #-}
