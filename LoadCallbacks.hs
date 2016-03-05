{- |
Module      : LoadCallbacks
Description : Loads callbacks from file (needed due to TH stage restriction).
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module LoadCallbacks where

import Bio.Motions.Callback.Parser.TH

[callbacksFile|config/callbacks|]
