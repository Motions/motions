{- |
Module      : Bio.Motions.Utils.Parsec
Description : Parsec utilities for common types.
License     : Apache
Stability   : experimental
Portability : unportable
-}
module Bio.Motions.Utils.Parsec where

import Bio.Motions.Types

import Text.Parsec as P
import Text.Parsec.ByteString
import GHC.Exts

energyVector :: Parser EnergyVector
energyVector = fromList <$> listOf int

listOf :: Parser a -> Parser [a]
listOf p = char '[' *> p `sepBy1` (spaces *> char ',') <* char ']'

int :: Parser Int
int = read <$> many1 digit <?> "non-negative integer"
