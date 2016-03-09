{- |
Module      : Bio.Motions.Utils.Parsec
Description : Parsec utilities for common types.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE OverloadedLists #-}
module Bio.Motions.Utils.Parsec where

import Bio.Motions.Types

import qualified Data.Vector.Unboxed as U
import Text.ParserCombinators.Parsec

energyVector :: Parser EnergyVector
energyVector = EnergyVector . U.fromList <$> listOf int

listOf :: Parser a -> Parser [a]
listOf p = char '[' *> p `sepBy1` (spaces *> char ',') <* char ']'

int :: Parser Int
int = (read <$> many1 digit) <?> "non-negative integer"

eol :: Parser String
eol = try (string "\n\r")
     <|> try (string "\r\n")
     <|> string "\r"
     <|> string "\n"
     <?> "End of line"
