{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Module      : Bio.Motions.Callback.Class
Description : Contains the definition and helper function of CalbackSerialisable class.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
module Bio.Motions.Callback.Serialisation where

import Bio.Motions.Types
import Bio.Motions.Representation.Class
import qualified Bio.Motions.Format.Proto.Callback as Proto
import qualified Bio.Motions.Format.Proto.Callback.Type as Proto

import Data.Proxy
import Data.Sequence as S
import Text.ProtocolBuffers.Basic

-- |Represents value that can be serialised as Callback Protos
class CallbackSerialisable cb where
    serialiseCallback :: String -> cb -> Proto.Callback
    prettyPrintCallback :: cb -> String

serialiseInt :: String -> Int -> Proto.Callback
serialiseInt name value = Proto.Callback
  { Proto.name = Just $ uFromString name
  , Proto.int_value = Just $ fromIntegral value
  , Proto.double_value = Nothing
  , Proto.int_list_value = empty
  , Proto.double_list_value = empty
  , Proto.callback_type = Just Proto.INT
  }

serialiseDouble :: String -> Double -> Proto.Callback
serialiseDouble name value = Proto.Callback
  { Proto.name = Just $ uFromString name
  , Proto.int_value = Nothing
  , Proto.double_value = Just $ value
  , Proto.int_list_value = empty
  , Proto.double_list_value = empty
  , Proto.callback_type = Just Proto.DOUBLE
  }


serialiseListInt :: String -> [Int] -> Proto.Callback
serialiseListInt name value = Proto.Callback
  { Proto.name = Just $ uFromString name
  , Proto.int_value = Nothing
  , Proto.double_value = Nothing
  , Proto.int_list_value = S.fromList $ map fromIntegral value
  , Proto.double_list_value = empty
  , Proto.callback_type = Just Proto.INT_LIST
  }


serialiseListDouble :: String -> [Double] -> Proto.Callback
serialiseListDouble name value = Proto.Callback
  { Proto.name = Just $ uFromString name
  , Proto.int_value = Nothing
  , Proto.double_value = Nothing
  , Proto.int_list_value = empty
  , Proto.double_list_value = S.fromList value
  , Proto.callback_type = Just Proto.DOUBLE_LIST
  }

instance CallbackSerialisable Int where
    serialiseCallback = serialiseInt
    prettyPrintCallback = show

instance CallbackSerialisable Double where
    serialiseCallback = serialiseDouble
    prettyPrintCallback = show

instance CallbackSerialisable [Int] where
    serialiseCallback = serialiseListInt
    prettyPrintCallback = show

instance CallbackSerialisable [Double] where
    serialiseCallback = serialiseListDouble
    prettyPrintCallback = show
