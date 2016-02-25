{- |
Module      : Bio.Motions.Utils.FreezePredicateParser
Description : Contains a parser of 'FreezePredicate's.
License     : Apache
Stability   : experimental
Portability : portable
-}

module Bio.Motions.Utils.FreezePredicateParser(freezePredicateParser) where

import Bio.Motions.Types
import Bio.Motions.Representation.Common
import Control.Lens
import Control.Monad
import Text.ParserCombinators.Parsec

-- |Parses a freeze predicate.
--
-- Grammar:
--
--   * @expr ::= literal | literal,expr@
--   * @literal ::= term | !literal@
--   * @term ::= (expr) | rangeOrWildcard | rangeOrWildcard:rangeOrWildcard@
--   * @rangeOrWildcard = * | integer | integer-integer@
--
-- @!@ denotes negation, @,@ denotes alternative, @*@ matches everything.
--
-- Examples:
--
--   * @2:1-100,9@ freezes beads from 1 to 100 on the chain 2, and also the whole chain 9
--   * @*:0@ freezes the 0th bead of every chain
--   * @!(4,5)@ freezes everything except for chains 4 and 5
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
rangeOrWildcard = valueOrRange <|> (char '*' >> pure (const True))

valueOrRange :: Parser (Int -> Bool)
valueOrRange = do
    lower <- integer
    option (== lower) $ do
        char '-'
        upper <- integer
        pure $ (&&) <$> (lower <=) <*> (<= upper)

integer :: Parser Int
integer = read <$> many1 digit
