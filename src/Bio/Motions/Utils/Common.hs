{- |
Module      : Bio.Motions.Utils.Common
Description : Common utility functions and types.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Bio.Motions.Utils.Common where

import Control.Monad.Except
import Data.List
import Data.Map as M
import GHC.Exts

findOrError :: (MonadError e m, Ord k) => e -> k -> Map k v -> m v
findOrError e k = maybe (throwError e) pure . M.lookup k

-- |Performs a monadic action on every entry on the list that appears again later in the list.
onDuplicates :: (Eq a, Monad m) => [a] -> (a -> m ()) -> m ()
onDuplicates xs onDup = forM_ (init . tails $ xs) $ \(y:ys) -> when (y `elem` ys) $ onDup y

-- |All two-element subsets (returned as ordered pairs) of the given list.
distinctPairs :: [a] -> [(a, a)]
distinctPairs l = [(a, b) | a : tl <- init (tails l), b <- tl]

-- |Ensures that the 'c' is satisfied for every element of 'list'.
type family All (c :: k -> Constraint) (list :: [k]) :: Constraint where
    All c '[] = ()
    All c (x ': xs) = (c x, All c xs)
