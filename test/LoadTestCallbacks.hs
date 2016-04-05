{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module LoadTestCallbacks where

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Serialisation
import Bio.Motions.Callback.Parser.TH
import Bio.Motions.Callback.Periodic

[callback|CALLBACK "sum42-beads"
    EVERY ACCEPTED 1
    NODES 1
    WHERE BELONGS(X 0, BEAD_BINDING_TO 0) OR BELONGS(X 0, BEAD_BINDING_TO 1)
    COMPUTE SUM 42
|]

[callback|CALLBACK "prod2-all"
    EVERY ACCEPTED 1
    NODES 1
    WHERE 1 == 1
    COMPUTE PRODUCT 2
|]

[callback|CALLBACK "list42-binders"
    EVERY ACCEPTED 1
    NODES 1
    WHERE BELONGS(X 0, BINDER 0) OR BELONGS(X 0, BINDER 1)
    COMPUTE LIST 42
|]

[callback|CALLBACK "pairs-dist<2"
    EVERY ACCEPTED 1
    NODES 2
    WHERE DIST(X 0, X 1) * DIST(X 0, X 1) < 2
    COMPUTE SUM 1
|]

[callback|CALLBACK "complex-function"
    EVERY ACCEPTED 1
    NODES 5
    WHERE BELONGS(X 0, BINDER 1) AND BELONGS(X 4, BEAD_BINDING_TO 0) AND DIST(X 1, X 3) <= 1.5
    COMPUTE SUM DIST(X 0, X 1) + DIST(X 1, X 4) * DIST(X 3, X 2) - DIST(X 4, X 0)
|]

[callbacksFile|test/testdata/callbacks|]

data EmptyCallback
deriving instance Show EmptyCallback

instance CallbackSerialisable EmptyCallback where
    serialiseCallback s = error $ "Empty Callback"
    prettyPrintCallback _ = "_empty"

instance Callback 'Pre EmptyCallback where
    callbackName _ = "_empty"
    runCallback = undefined
    updateCallback = undefined

type instance CallbackPeriod EmptyCallback = 3
