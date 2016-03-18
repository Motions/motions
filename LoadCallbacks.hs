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
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module LoadCallbacks where

import Bio.Motions.Callback.Parser.TH

[callbacksFile|config/callbacks|]
