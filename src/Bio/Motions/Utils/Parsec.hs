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
energyVector = EnergyVector . U.fromList <$> listOfInts

listOfInts :: Parser [Int]
listOfInts = char '[' *> int `sepBy1` (spaces *> char ',') <* char ']'

int :: Parser Int
int = (read <$> many1 digit) <?> "non-negative integer"

eol :: Parser String
eol = try (string "\n\r")
     <|> try (string "\r\n")
     <|> string "\r"
     <|> string "\n"
     <?> "End of line"
