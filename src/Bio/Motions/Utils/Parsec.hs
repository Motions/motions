{- |
Module      : Bio.Motions.Utils.Parsec
Description : Parsec utilities for common types.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE FlexibleContexts #-}
module Bio.Motions.Utils.Parsec where

import Bio.Motions.Types

import Text.Parsec
import Text.Parsec.Text
import GHC.Exts

word :: Stream s m Char => ParsecT s u m String
word = manyTill anyChar (lookAhead space)

energyVector :: Parser EnergyVector
energyVector = fromList <$> listOf int

listOf :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
listOf p = char '[' *> p `sepBy1` (spaces *> char ',') <* char ']'

int :: Stream s m Char => ParsecT s u m Int
int = read <$> many1 digit <?> "non-negative integer"
