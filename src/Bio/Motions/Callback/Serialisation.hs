{- |
Module      : Bio.Motions.Callback.Class
Description : Contains the definition and helper function of CallbackSerialisable class.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Bio.Motions.Callback.Serialisation where

import qualified Bio.Motions.Format.Proto.Callback as Proto
import qualified Bio.Motions.Format.Proto.Callback.Type as Proto

import Data.Sequence as S
import Text.ProtocolBuffers.Basic

-- |Represents values that can be serialised as Callback Protos
class CallbackSerialisable cb where
    -- |Serialise as a Protocol Buffer message
    serialiseCallback ::
        String
        -- ^Name
        -> cb
        -- ^Value
        -> Maybe Proto.Callback
        -- ^Serialised value, or nothing if the callback
        -- hasn't been computed in this step

serialiseInt :: String -> Int -> Maybe Proto.Callback
serialiseInt name value = Just Proto.Callback
    { Proto.name = Just $ uFromString name
    , Proto.int_value = Just $ fromIntegral value
    , Proto.double_value = Nothing
    , Proto.int_list_value = empty
    , Proto.double_list_value = empty
    , Proto.callback_type = Just Proto.INT
    }

serialiseDouble :: String -> Double -> Maybe Proto.Callback
serialiseDouble name value = Just Proto.Callback
    { Proto.name = Just $ uFromString name
    , Proto.int_value = Nothing
    , Proto.double_value = Just value
    , Proto.int_list_value = empty
    , Proto.double_list_value = empty
    , Proto.callback_type = Just Proto.DOUBLE
    }

serialiseListInt :: String -> [Int] -> Maybe Proto.Callback
serialiseListInt name value = Just Proto.Callback
    { Proto.name = Just $ uFromString name
    , Proto.int_value = Nothing
    , Proto.double_value = Nothing
    , Proto.int_list_value = S.fromList $ map fromIntegral value
    , Proto.double_list_value = empty
    , Proto.callback_type = Just Proto.INT_LIST
    }

serialiseListDouble :: String -> [Double] -> Maybe Proto.Callback
serialiseListDouble name value = Just Proto.Callback
    { Proto.name = Just $ uFromString name
    , Proto.int_value = Nothing
    , Proto.double_value = Nothing
    , Proto.int_list_value = empty
    , Proto.double_list_value = S.fromList value
    , Proto.callback_type = Just Proto.DOUBLE_LIST
    }

instance CallbackSerialisable Int where
    serialiseCallback = serialiseInt

instance CallbackSerialisable Double where
    serialiseCallback = serialiseDouble

instance CallbackSerialisable [Int] where
    serialiseCallback = serialiseListInt

instance CallbackSerialisable [Double] where
    serialiseCallback = serialiseListDouble
