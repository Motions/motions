module Bio.Motions.Utils.Common where

import Data.Map as M

findOrError :: Ord k => e -> k -> Map k v -> Either e v
findOrError e k = maybe (Left e) Right . M.lookup k
