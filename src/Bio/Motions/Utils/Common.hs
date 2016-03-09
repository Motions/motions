{- |
Module      : Bio.Motions.Utils.Common
Description : Common utility functions.
License     : Apache
Stability   : experimental
Portability : unportable
-}
module Bio.Motions.Utils.Common where

import Control.Monad.Except
import Data.Map as M

findOrError :: (MonadError e m, Ord k) => e -> k -> Map k v -> m v
findOrError e k = maybe (throwError e) pure . M.lookup k
