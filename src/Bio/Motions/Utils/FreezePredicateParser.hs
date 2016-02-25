{- |
Module      : Bio.Motions.Utils.FreezePredicateParser
Description : Contains a parser of 'FreezePredicate's.
License     : MIT
Stability   : experimental
Portability : portable
-}

module Bio.Motions.Utils.FreezePredicateParser(freezePredicateParser) where

import Bio.Motions.Types
import Bio.Motions.Representation.Common
import Control.Lens
import Control.Monad
import Text.ParserCombinators.Parsec

freezePredicateParser :: Parser FreezePredicate
freezePredicateParser = expr <* eof

expr :: Parser FreezePredicate
expr = chainl literal (char ',' >> pure (liftM2 (||))) freezeNothing

literal :: Parser FreezePredicate
literal = term <|> (char '!' >> liftM not <$> literal)

term :: Parser FreezePredicate
term = between (char '(') (char ')') expr <|> do
    chainPredicate <- rangeOrWildcard
    let chainOk x = chainPredicate $ x ^. beadChain
    option chainOk $ do
        char ':'
        indexPredicate <- rangeOrWildcard
        let indexOk x = indexPredicate $ x ^. beadIndexOnChain
        pure $ (&&) <$> chainOk <*> indexOk

rangeOrWildcard :: Parser (Int -> Bool)
rangeOrWildcard = range <|> (char '*' >> pure (const True))

range :: Parser (Int -> Bool)
range = do
    lower <- integer
    option (== lower) $ do
        char '-'
        upper <- integer
        pure $ (&&) <$> (lower <=) <*> (<= upper)

integer :: Parser Int
integer = read <$> many1 digit
