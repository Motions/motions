{- |
Module      : Bio.Motions.Callback.Parser.TH
Description : Contains the Template Haskell stuff.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Bio.Motions.Callback.Parser.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Bio.Motions.Callback.Class
import Bio.Motions.Callback.Parser.Parser
import Bio.Motions.Representation.Class
import Bio.Motions.Types
import Control.Lens
import Control.Monad.State.Strict
import Data.Foldable
import Data.Traversable
import Data.MonoTraversable
import Data.Monoid
import Linear
import qualified Text.Parsec as P
import Data.Proxy
import qualified GHC.TypeLits as TL
import GHC.Prim

-- |Represents the return value type of a callback.
type family THCallbackResult (name :: TL.Symbol) :: *

-- |Represents the callback arity, i.e. the number of
-- node arguments.
type family THCallbackArity (name :: TL.Symbol) :: Nat

type role THCallback nominal
-- |A wrapper around the callback return type, which will be provided an instance
-- of 'Callback'.
newtype THCallback (name :: TL.Symbol) = THCallback { getTHCallback :: THCallbackResult name }

deriving instance Eq (THCallbackResult name) => Eq (THCallback name)
deriving instance Ord (THCallbackResult name) => Ord (THCallback name)
deriving instance Num (THCallbackResult name) => Num (THCallback name)
deriving instance Enum (THCallbackResult name) => Enum (THCallback name)
deriving instance Real (THCallbackResult name) => Real (THCallback name)
deriving instance Integral (THCallbackResult name) => Integral (THCallback name)
deriving instance Show (THCallbackResult name) => Show (THCallback name)
deriving instance Read (THCallbackResult name) => Read (THCallback name)

-- |An auxiliary class used for lifting types into type expressions.
class LiftProxy a where
    liftProxy :: Proxy# a -> TypeQ

instance LiftProxy Int where
    liftProxy _ = [t| Int |]

instance LiftProxy Double where
    liftProxy _ = [t| Double |]

instance LiftProxy Bool where
    liftProxy _ = [t| Bool |]

instance LiftProxy Zero where
    liftProxy _ = [t| Zero |]

instance LiftProxy n => LiftProxy (Succ n) where
    liftProxy _ = [t| Succ $(liftProxy (proxy# :: Proxy# n)) |]

-- |Convenient alias.
type LiftsA = Both Lift LiftProxy

-- |Convenient alias.
type LiftsN = Both ForEachKNodes LiftProxy

-- |Creates Template Haskell declarations from a suitable 'ParsedCallback'.
createCallback :: forall n a. (ForEachKNodes n, LiftProxy a, LiftProxy n)
    => ParsedCallback LiftsA n a -> Q [Dec]
createCallback ParsedCallback{..} = do
    common <- [d|
        type instance THCallbackArity $(name) = $(liftProxy (proxy# :: Proxy# n))
        type instance THCallbackResult $(name) = $(constrT $ liftProxy (proxy# :: Proxy# a))

        instance Monad m => Callback m 'Post (THCallback $(name)) where
            runCallback repr = forEachKNodes repr run
              where
                run :: Vec (THCallbackArity $(name)) Atom -> m (THCallback $(name))
                run args =  pure $
                    if $(unTypeQ $ ev callbackCondition) then
                        THCallback $(constrE $ unTypeQ $ ev expr)
                    else
                        mempty
        |]

    monoid <- case callbackResult of
        CallbackSum _ -> [d|
            instance Monoid (THCallback $(name)) where
                mempty = 0
                {-# INLINE mempty #-}
                mappend = (+)
                {-# INLINE mappend #-}
            |]
        CallbackProduct _ -> [d|
            instance Monoid (THCallback $(name)) where
                mempty = 1
                {-# INLINE mempty #-}
                mappend = (*)
                {-# INLINE mappend #-}
            |]
        CallbackList _ -> [d|
            instance Monoid (THCallback $(name)) where
                mempty = THCallback []
                {-# INLINE mempty #-}
                (THCallback x) `mappend` (THCallback y) = THCallback $ x ++ y
                {-# INLINE mappend #-}
            |]

    pure $ common ++ monoid
  where
    name = litT $ strTyLit callbackName
    ev x = eval EvalCtx
                    { evalCtxArgs = unsafeTExpCoerce $ varE $ mkName "args"
                    , evalCtxRepr = mkName "repr"
                    } x

    (constrE, constrT, expr) = case callbackResult of
        CallbackSum expr -> (id, id, expr)
        CallbackProduct expr -> (id, id, expr)
        CallbackList expr -> (listE . (:[]), appT listT, expr)

-- |A callback quasiquoter, accepting any suitable callback with arity strictly less than 'maxn'.
quoteCallback :: MaxNConstraint LiftsA LiftsN maxn => Proxy# maxn -> String -> Q [Dec]
quoteCallback p str =
    case P.parse (parseCallback p) "TH" str of
        Right ((ParsedCallbackWrapper exp) :: ParsedCallbackWrapper LiftsA LiftsN) ->
            createCallback exp
        Left err -> fail $ show err

-- |A callback quasiquoter
callback = QuasiQuoter
    { quoteDec = quoteCallback (proxy# :: Proxy# (ToNat 10))
    }

-- |A fixed-width vector
data Vec (n :: Nat) a where
    Nil :: Vec Zero a
    Cons :: a -> Vec n a -> Vec (Succ n) a

-- |Evaluation context
data EvalCtx n = EvalCtx
    { evalCtxArgs :: Q (TExp (Vec n Atom)) -- ^A typed expression representing the arguments vector
    , evalCtxRepr :: Name -- ^The name of the binding of the representation
    }

-- |Convert 'TL.Nat' to 'Nat'.
type family ToNat (n :: TL.Nat) :: Nat where
    ToNat 0 = Zero
    ToNat n = Succ (ToNat (n TL.- 1))

-- |Type-safe !! on 'Vec'tors.
access :: Node n -> Vec n a -> a
access FirstNode (Cons h _) = h
access (NextNode n) (Cons _ t) = access n t
{-# INLINE access #-}

-- |A transformer of 'Expr' into typed expressions.
eval :: EvalCtx n -> Expr LiftsA n a -> Q (TExp a)
eval ctx (EAnd lhs rhs) =  [|| $$(eval ctx lhs) && $$(eval ctx rhs) ||]
eval ctx (EOr lhs rhs) =   [|| $$(eval ctx lhs) || $$(eval ctx rhs) ||]
eval ctx (ENot lhs) =      [|| not $$(eval ctx lhs) ||]

eval ctx (ELt lhs rhs) =   [|| $$(eval ctx lhs) <  $$(eval ctx rhs) ||]
eval ctx (ELte lhs rhs) =  [|| $$(eval ctx lhs) <= $$(eval ctx rhs) ||]
eval ctx (EGt lhs rhs) =   [|| $$(eval ctx lhs) >  $$(eval ctx rhs) ||]
eval ctx (EGte lhs rhs) =  [|| $$(eval ctx lhs) >= $$(eval ctx rhs) ||]
eval ctx (EEq lhs rhs) =   [|| $$(eval ctx lhs) == $$(eval ctx rhs) ||]
eval ctx (ENeq lhs rhs) =  [|| $$(eval ctx lhs) /= $$(eval ctx rhs) ||]

eval ctx (EInt lhs) =      [|| floor $$(eval ctx lhs) ||]
eval ctx (EFlt lhs) =      [|| fromIntegral $$(eval ctx lhs) ||]

eval ctx (EAdd lhs rhs) =  [|| $$(eval ctx lhs) + $$(eval ctx rhs) ||]
eval ctx (ESub lhs rhs) =  [|| $$(eval ctx lhs) - $$(eval ctx rhs) ||]
eval ctx (EMul lhs rhs) =  [|| $$(eval ctx lhs) * $$(eval ctx rhs) ||]
eval ctx (EDiv lhs rhs) =  [|| $$(eval ctx lhs) / $$(eval ctx rhs) ||]
eval ctx (EIDiv lhs rhs) = [|| $$(eval ctx lhs) `div` $$(eval ctx rhs) ||]
eval ctx (EMod lhs rhs) =  [|| $$(eval ctx lhs) `mod` $$(eval ctx rhs) ||]

eval ctx (EMin lhs rhs) =  [|| $$(eval ctx lhs) `min` $$(eval ctx rhs) ||]
eval ctx (EMax lhs rhs) =  [|| $$(eval ctx lhs) `max` $$(eval ctx rhs) ||]

eval EvalCtx{..} (EBelongs node cls) = [||
    case (cls, access node $$(evalCtxArgs)) of
        (AtomTypeBead x, Bead BeadInfo{..}) -> x == beadType
        (AtomTypeBinder x, Binder BinderInfo{..}) -> x == binderType
        _ -> False
    ||]


eval EvalCtx{..} (EDist lhs rhs) = [||
    sqrt $ fromIntegral $ qd
        (access lhs $$(evalCtxArgs) ^. position)
        (access rhs $$(evalCtxArgs) ^. position)
    ||]

eval _ (ELit lit) = [|| lit ||]

eval ctx EGr = undefined -- TODO
eval EvalCtx{..} (EAtomIx node) = [||
    case access node $$(evalCtxArgs) of
        Bead BeadInfo{..} -> beadAtomIndex
        _ -> -1
    ||]
eval EvalCtx{..} (EChainIx node) = [||
    case access node $$(evalCtxArgs) of
        Bead BeadInfo{..} -> beadChain
        _ -> -1
    ||]
eval EvalCtx{..} (EChromoIx node) = [||
    case access node $$(evalCtxArgs) of
        Bead BeadInfo{..} -> beadIndexOnChain
        _ -> -1
    ||]

instance Lift AtomType where
    lift (AtomTypeBead cls)   = [| AtomTypeBead cls |]
    lift (AtomTypeBinder cls) = [| AtomTypeBinder cls |]

instance Lift BeadType where
    lift (BeadType t) = [| BeadType t |]

instance Lift BinderType where
    lift (BinderType t) = [| BinderType t |]

instance Lift (Node n) where
    lift FirstNode = [| FirstNode |]
    lift (NextNode n) = [| NextNode n |]

-- |A helper class used to iterate over fixed-width vectors
-- of nodes.
class ForEachKNodes (n :: Nat) where
    forEachKNodes :: (Monoid r, ReadRepresentation m repr, Monad m)
        => repr -> (Vec n Atom -> m r) -> m r

-- |The base case.
instance ForEachKNodes Zero where
    forEachKNodes _ fun = fun Nil
    {-# INLINE forEachKNodes #-}

-- |The recursive case.
instance ForEachKNodes n => ForEachKNodes (Succ n) where
    forEachKNodes repr fun = forEachKNodes repr $ \xs ->
        forEachNode repr $ \x -> fun $ Cons x xs
    {-# INLINE forEachKNodes #-}

-- |Performs a monadic action over all nodes (i.e. beads and atoms)
-- and gathers the results monoidally.
forEachNode :: forall m r repr. (Monoid r, ReadRepresentation m repr, Monad m)
    => repr -> (Atom -> m r) -> m r
forEachNode repr f = do
    numChains <- getNumberOfChains repr
    binders <- getBinders repr $ go Binder
    beads <- fold <$> traverse (\idx -> getChain repr idx $ go Bead) [0..numChains-1]
    pure $ beads <> binders
  where
    go :: (MonoTraversable c, Element c ~ a) => (a -> Atom) -> c -> m r
    go conv = flip ofoldlM mempty $ \s x -> mappend s <$> f (conv x)
{-# INLINE forEachNode #-}
