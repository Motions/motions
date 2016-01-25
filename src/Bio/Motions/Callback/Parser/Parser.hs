{- |
Module      : Bio.Motions.Callback.Parser.Parser
Description : Contains the definitions of various 'Callback' parsing-related types.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bio.Motions.Callback.Parser.Parser
    ( CallbackFrequency(..)
    , AtomType(..)
    , CallbackResult(..)
    , ParsedCallback(..)
    , ParsedCallbackWrapper(..)
    , MaxNConstraint
    , Both
    , Nat(..)
    , Expr(..)
    , Node(..)
    , EC
    , ToNode(..)
    , parseCallback) where

import Data.Maybe
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)
import GHC.Prim
import Bio.Motions.Types

-- |Represents the frequency a callback has to be run
data CallbackFrequency = EveryNFrames Int | EveryNAcceptedFrames Int

-- |Represents the type of an atom
data AtomType = AtomTypeBinder BinderType
              -- ^ A binder
              | AtomTypeBeadBindingTo BinderType
              -- ^ A bead with non-zero value for the given 'BinderType'
              -- in its 'EnergyVector'.

-- |The return value of a callback
data CallbackResult c n a where
    CallbackSum     :: Expr c n a -> CallbackResult c n a
    CallbackProduct :: Expr c n a -> CallbackResult c n a
    CallbackList    :: Expr c n a -> CallbackResult c n a

-- |Represents a parsed callback
data ParsedCallback c n a = ParsedCallback
    { callbackFrequency :: CallbackFrequency
    , callbackName :: String
    , callbackCondition :: Expr c n Bool
    -- ^The result should be computed only for tuples of atoms that satisfy the specified condition.
    , callbackResult :: CallbackResult c n a
    }

-- |Natural numbers
data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat
    deriving (Eq, Show)

-- |A wrapper around node argument identifiers.
--
-- 'Node' 'n' keeps a natural number that is guaranteed to be less than 'n'.
data Node (n :: Nat) where
    FirstNode :: Node (Succ n)
    NextNode :: Node n -> Node (Succ n)

-- |Convert runtime natural 'Int's to type-level naturals.
class ToNode (n :: Nat) where
    -- |Converts an non-negative 'Int' to a 'Node' if it is strictly less than 'n'.
    -- Fails otherwise.
    toNode :: Int -> Maybe (Node n)

-- |No non-negative 'Int' is smaller than 'Zero'
instance ToNode Zero where
    toNode _ = Nothing

instance ToNode n => ToNode (Succ n) where
    toNode 0 = Just FirstNode
    toNode n | n < 0 = Nothing
    toNode n = NextNode <$> toNode (n - 1)

-- |A useful constraint. 'EC' 'c' 'n' '[a_0, a_1, ...] means:
-- 'ToNode' 'n' and all of 'c' a_0, 'c' a_1, ...
type family EC (c :: * -> Constraint) (n :: Nat) (a :: [*]) :: Constraint
type instance EC c n '[] = (ToNode n)
type instance EC c n (t ': ts) = (c t, EC c n ts)

-- |The AST of the callback DSL.
--
-- 'c' is a Constraint on the types of all subexpressions.
-- 'n' is the arity (i.e. the number of node arguments).
-- 'a' is the type the expression evaluates to.
data Expr c n a where
    EAnd  :: (EC c n '[Bool]) => Expr c n Bool -> Expr c n Bool -> Expr c n Bool
    EOr   :: (EC c n '[Bool]) => Expr c n Bool -> Expr c n Bool -> Expr c n Bool
    ENot  :: (EC c n '[Bool]) => Expr c n Bool -> Expr c n Bool

    ELt   :: (EC c n '[a, Bool], Ord a) => Expr c n a -> Expr c n a -> Expr c n Bool
    ELte  :: (EC c n '[a, Bool], Ord a) => Expr c n a -> Expr c n a -> Expr c n Bool
    EGt   :: (EC c n '[a, Bool], Ord a) => Expr c n a -> Expr c n a -> Expr c n Bool
    EGte  :: (EC c n '[a, Bool], Ord a) => Expr c n a -> Expr c n a -> Expr c n Bool
    EEq   :: (EC c n '[a, Bool], Eq a)  => Expr c n a -> Expr c n a -> Expr c n Bool
    ENeq  :: (EC c n '[a, Bool], Eq a)  => Expr c n a -> Expr c n a -> Expr c n Bool

    EDist :: (EC c n '[Double]) => Node n -> Node n -> Expr c n Double
    EInt  :: (EC c n '[a, Int], RealFrac a) => Expr c n a -> Expr c n Int
    EFlt  :: (EC c n '[Int, Double]) => Expr c n Int -> Expr c n Double
    EGr   :: (EC c n '[Double]) => Expr c n Double

    EAdd  :: (EC c n '[a], Num a) => Expr c n a -> Expr c n a -> Expr c n a
    ESub  :: (EC c n '[a], Num a) => Expr c n a -> Expr c n a -> Expr c n a
    EMul  :: (EC c n '[a], Num a) => Expr c n a -> Expr c n a -> Expr c n a
    EDiv  :: (EC c n '[a], Fractional a) => Expr c n a -> Expr c n a -> Expr c n a
    EIDiv :: (EC c n '[a], Integral a) => Expr c n a -> Expr c n a -> Expr c n a
    EMod  :: (EC c n '[a], Integral a) => Expr c n a -> Expr c n a -> Expr c n a

    EMin  :: (EC c n '[a], Ord a) => Expr c n a -> Expr c n a -> Expr c n a
    EMax  :: (EC c n '[a], Ord a) => Expr c n a -> Expr c n a -> Expr c n a

    ELit  :: (EC c n '[a]) => a -> Expr c n a

    EAtomIx   :: (EC c n '[Int]) => Node n -> Expr c n Int
    EChainIx  :: (EC c n '[Int]) => Node n -> Expr c n Int
    EChromoIx :: (EC c n '[Int]) => Node n -> Expr c n Int

    EBelongs  :: (EC c n '[Bool]) => Node n -> AtomType -> Expr c n Bool

-- |An alias, for simplicity.
type Parser a = Parsec String () a

-- |An existential type wrapper, hiding the arity and the result type.
data ParsedCallbackWrapper c cn where
    ParsedCallbackWrapper ::
        (EC c n '[Int, Double, Bool, a], cn n)
        => ParsedCallback c n a
        -> ParsedCallbackWrapper c cn

-- |Convert a term-level 'Int' into a type-level 'Nat' and perform an arbitrary computation
-- parameterized by such a 'Nat'. Only 'Nat's less than 'limit' are considered (if the provided 'Int'
-- is either negative or not less than 'limit', the function returns 'Nothing').
--
-- Since the computation to be performed (passed as a continuation) may require some constraint
-- on 'm' to hold, 'tryNatsBelow' is parameterized by an additional 'Constraint' constructor
-- @'c' :: 'Nat' -> 'Constraint'@.
--
-- Because the correct 'm' is not known statically, 'tryNatsBelow' requires @'c' 'm'@ to hold
-- for every 'm' less than 'limit'.
tryNatsBelow :: forall c limit r. (TryNatsBelow c limit)
    => Proxy# '(limit, c)
    -> Int
    -- ^A runtime natural number.
    -> (forall (m :: Nat). c m => Proxy# m -> r)
    -- ^The continuation to be called when the correct @'m' :: 'Nat'@ equal to the 'Int' provided
    -- is found.
    -> Maybe r
    -- ^Its return value iff the 'Int' provided is non-negative and less than 'limit',
    -- Nothing otherwise.
tryNatsBelow _ n f
    | n >= 0 = tryNatsBelow' (proxy# :: Proxy# '(Zero, limit, c)) n f
    | otherwise = Nothing

-- | See 'tryNatsBelow'.
--
-- This class performs one step of the 'Int'-to-'Nat' conversion, which is done by induction
-- on 'limit'.
--
-- It keeps the invariant: @n + 'acc' == result@, where n is the provided 'Int' and result is the
-- target 'Nat', where @n < 'limit'@.
class TryNatsBelow' c (limit :: Nat) (acc :: Nat) where
    tryNatsBelow' ::   Proxy# '(acc, limit, c)
                    -> Int -- ^ A non-negative integer
                    -> (forall (m :: Nat). c m => Proxy# m -> x)
                    -> Maybe x

-- | Conversion should start with the accumulator equal to 'Zero'
type TryNatsBelow c limit = TryNatsBelow' c limit 'Zero

-- |@n < 'Zero'@ -- absurd.
instance TryNatsBelow' c Zero acc where
    tryNatsBelow' _ _ _ = Nothing

-- | @(n + 1) + 'acc' == n + ('Succ' 'acc')@ and @(n + 1) < ('Succ' 'limit')@ iff @n < 'limit'@,
-- i.e. "move" one +1 from the term-level 'Int' to the type-level 'Nat'.
instance (c acc, TryNatsBelow' c limit (Succ acc)) => TryNatsBelow' c (Succ limit) acc where
    tryNatsBelow' _ 0 run = Just $ run (proxy# :: Proxy# acc)
    tryNatsBelow' _ n run = tryNatsBelow' (proxy# :: Proxy# '(Succ acc, limit, c)) (n - 1) run

-- |'EC' with its arguments flipped.
class EC c n a => ECc c a n
instance EC c n a => ECc c a n

-- |@Both c1 c2 a@ iff @(c1 a, c2 a)@.
class (c1 a, c2 a) => Both c1 c2 a
instance (c1 a, c2 a) => Both c1 c2 a

-- |A convenient wrapper. See 'parseCallback'.
type MaxNConstraint c cn maxn = ( EC c maxn '[Int, Double, Bool]
                                , TryNatsBelow (Both (ECc c '[Int, Double, Bool]) cn) maxn
                                )

-- |Parses a callback with arity strictly less than 'maxn', whose internal types
-- must satisfy the constraint 'c'. The callback is parsed as Int-returning where possible,
-- Double-returning otherwise. Moreover, it is required that all all numbers between
-- 'Zero' (incl.) and 'maxn' (excl.) satsify the constraint 'cn'.
parseCallback :: forall c cn maxn. MaxNConstraint c cn maxn
    => Proxy# maxn -> Parser (ParsedCallbackWrapper c cn)
parseCallback _ = do
    reserved "CALLBACK"
    name <- stringLiteral

    reserved "EVERY"
    freq <- (reserved "ACCEPTED" >> EveryNAcceptedFrames . fromIntegral <$> natural)
         <|> EveryNFrames . fromIntegral <$> natural

    reserved "NODES"
    arity <- fromIntegral <$> natural
    fromMaybe (fail $ "The arity " ++ show arity ++ " exceeds the bound") $
        tryNatsBelow (proxy# :: Proxy# '(maxn, Both (ECc c '[Int, Double, Bool]) cn)) arity $
            withArity name freq
  where
    -- | Parses a callback from the point the arity is known (at the type-level).
    withArity :: forall n. (EC c n '[Int, Double, Bool], cn n) => String -> CallbackFrequency
        -> Proxy# n -> Parser (ParsedCallbackWrapper c cn)
    withArity name freq _ = do
        reserved "WHERE"
        cond <- expr :: Parser (Expr c n Bool)

        reserved "COMPUTE"

        try (finish name freq cond (withArityAndType :: Parser (CallbackResult c n Int)))
            <|> finish name freq cond (withArityAndType :: Parser (CallbackResult c n Double))

    withArityAndType :: (EC c n '[Int, Double, Bool, a], Parseable c n a)
        => Parser (CallbackResult c n a)
    withArityAndType = (reserved "SUM" >> CallbackSum <$> expr)
                   <|> (reserved "PRODUCT" >> CallbackProduct <$> expr)
                   <|> (reserved "LIST" >> CallbackList <$> expr)

    finish :: (EC c n '[a], cn n) => String -> CallbackFrequency -> Expr c n Bool
        -> Parser (CallbackResult c n a) -> Parser (ParsedCallbackWrapper c cn)
    finish name freq cond parser = do
        result <- parser

        pure $ ParsedCallbackWrapper ParsedCallback
            { callbackFrequency = freq
            , callbackName = name
            , callbackCondition = cond
            , callbackResult = result
            }

-- |Parses an expression, where @expr ::= 'term' | 'term' 'addop' expr@
expr :: Parseable c n a => Parser (Expr c n a)
expr = term `chainl1` addop

-- |Parses a term, where @term ::= 'factor' | 'factor' 'mulop' term@
term :: Parseable c n a => Parser (Expr c n a)
term = factor `chainl1` mulop

-- |Parses a factor, where @factor ::= (expr) | 'atom'@.
factor :: Parseable c n a => Parser (Expr c n a)
factor = parens expr <|> atom

-- |An auxiliary class providing common parsers.
class EC c n '[a] => Parseable c n a where
    -- |Parses an atom.
    atom :: Parser (Expr c n a)

    -- |Parses an additive binary operator.
    addop :: Parser (Expr c n a -> Expr c n a -> Expr c n a)

    -- |Parses a multiplicative binary operator.
    mulop :: Parser (Expr c n a -> Expr c n a -> Expr c n a)

-- |Integral expressions.
instance EC c n '[Int, Double] => Parseable c n Int where
    addop =   (reservedOp "+" >> pure EAdd)
          <|> (reservedOp "-" >> pure ESub)

    mulop =   (reservedOp "*"  >> pure EMul)
          <|> (reservedOp "//" >> pure EIDiv)
          <|> (reservedOp "%"  >> pure EMod)

    atom  =   literal
          <|> (reserved "ATOM_INDEX"  >> EAtomIx   <$> parens constant)
          <|> (reserved "CHAIN_INDEX" >> EChainIx  <$> parens constant)
          <|> (reserved "CHROMOSOME"  >> EChromoIx <$> parens constant)
          <|> (reserved "INT" >> parens int)
          <|> (reserved "MIN" >> parens (EMin <$> expr <* comma <*> expr))
          <|> (reserved "MAX" >> parens (EMax <$> expr <* comma <*> expr))
            where
              int =   try expr
                  <|> EInt <$> (expr :: Parser (Expr c n Double))

-- |Floating-point expressions.
instance EC c n '[Int, Double] => Parseable c n Double where
    addop =   (reservedOp "+" >> pure EAdd)
          <|> (reservedOp "-" >> pure ESub)

    mulop =   (reservedOp "*"  >> pure EMul)
          <|> (reservedOp "/"  >> pure EDiv)

    atom  =   literal
          <|> (reserved "GR" >> parens (pure EGr))
          <|> (reserved "DIST" >> parens (EDist <$> constant <* comma <*> constant))
          <|> (reserved "MIN" >> parens (EMin <$> expr <* comma <*> expr))
          <|> (reserved "MAX" >> parens (EMax <$> expr <* comma <*> expr))
          <|> (reserved "FLT" >> parens flt)
            where
              flt =   try expr
                  <|> EFlt <$> (expr :: Parser (Expr c n Int))

-- |Boolean expressions.
instance EC c n '[Bool, Int, Double] => Parseable c n Bool where
    addop = reserved "OR" >> pure EOr

    mulop = reserved "AND" >> pure EAnd

    atom =   (reserved "NOT" >> ENot <$> atom)
         <|> (reserved "BELONGS" >> parens (EBelongs <$> constant <* comma <*> constant))
         <|> (try (cmp (expr :: Parser (Expr c n Int)))
              <|>  cmp (expr :: Parser (Expr c n Double)))
         where
            cmp sub = do
                lhs <- sub
                op <-  choice
                       [ reservedOp "==" >> pure EEq
                       , reservedOp "!=" >> pure ENeq
                       , reservedOp "<"  >> pure ELt
                       , reservedOp "<=" >> pure ELte
                       , reservedOp ">"  >> pure EGt
                       , reservedOp ">=" >> pure EGte
                       ]
                op lhs <$> sub

-- |A literal.
literal :: (EC c n '[a], ParseConstant a) => Parser (Expr c n a)
literal = ELit <$> constant

-- |A helper class providing 'constant' parsers.
class ParseConstant a where
    -- |Parses an immediate constant.
    constant :: Parser a

-- |Integral constants.
instance ParseConstant Int where
    constant = fromIntegral <$> integer

deriving instance ParseConstant BinderType

-- |Floating-point or integral constants.
instance ParseConstant Double where
    constant = try float
             <|> fromIntegral <$> integer

-- |Node (parameter) identifier constants.
instance ToNode n => ParseConstant (Node n) where
    constant = do
        reserved "X"
        val <- constant
        case toNode val of
            Just node -> pure node
            Nothing -> fail $ "Out of bounds: " ++ show val

-- |Atom class constants.
instance ParseConstant AtomType where
    constant =   (reserved "BEAD_BINDING_TO"
                    >> AtomTypeBeadBindingTo <$> constant)
             <|> (reserved "BINDER"
                    >> AtomTypeBinder        <$> constant)

-- |The language definition.
dslDef :: P.LanguageDef st
dslDef = javaStyle
         { P.reservedOpNames = ["+", "-", "/", "*", "%", "//", "<=", ">=", "==", "!=", "<", ">"]
         , P.reservedNames = ["AND", "OR", "NOT", "DIST", "INT", "GR", "ATOM_INDEX", "CHAIN_INDEX",
                             "CHROMOSOME", "MIN", "MAX", "BELONGS", "X", "BEAD", "BINDER", "CALLBACK",
                             "EVERY", "ACCEPTED", "NODES", "WHERE", "COMPUTE", "SUM", "PRODUCT", "LIST"]
         }

-- |The token parser.
P.TokenParser{..} = P.makeTokenParser dslDef
