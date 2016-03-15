{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module LoadTestCallbacks where

import Bio.Motions.Callback.Parser.TH

[callback|CALLBACK "sum42-beads"
    EVERY 1
    NODES 1
    WHERE BELONGS(X 0, BEAD_BINDING_TO 0) OR BELONGS(X 0, BEAD_BINDING_TO 1)
    COMPUTE SUM 42
|]

[callback|CALLBACK "prod2-all"
    EVERY 1
    NODES 1
    WHERE 1 == 1
    COMPUTE PRODUCT 2
|]

[callback|CALLBACK "list42-binders"
    EVERY 1
    NODES 1
    WHERE BELONGS(X 0, BINDER 0) OR BELONGS(X 0, BINDER 1)
    COMPUTE LIST 42
|]

[callback|CALLBACK "pairs-dist<2"
    EVERY 1
    NODES 2
    WHERE DIST(X 0, X 1) * DIST(X 0, X 1) < 2
    COMPUTE SUM 1
|]

[callback|CALLBACK "complex-function"
    EVERY 1
    NODES 5
    WHERE BELONGS(X 0, BINDER 1) AND BELONGS(X 4, BEAD_BINDING_TO 0) AND DIST(X 1, X 3) <= 1.5
    COMPUTE SUM DIST(X 0, X 1) + DIST(X 1, X 4) * DIST(X 3, X 2) - DIST(X 4, X 0)
|]

[callbacksFile|test/testdata/callbacks|]
