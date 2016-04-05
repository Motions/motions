{- |
Module      : Bio.Motions.Callback.Parser.TH
Description : Contains the Template Haskell stuff.
License     : Apache
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
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Bio.Motions.Callback.Parser.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Bio.Motions.Callback.Class hiding (CallbackResult)
import Bio.Motions.Callback.Parser.Parser
import Bio.Motions.Callback.Periodic
import Bio.Motions.Callback.Serialisation
import Bio.Motions.Representation.Class
import Bio.Motions.Types
import Bio.Motions.Common
import Control.Lens
import Data.Foldable
import Data.Maybe
import Data.MonoTraversable
import Data.Monoid
import Linear
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified GHC.TypeLits as TL
import GHC.Prim

class IsTHCallback (name :: TL.Symbol) where
    -- |Represents the return value type of a callback.
    type THCallbackResult name :: *

    -- |Represents the callback arity, i.e. the number of
    -- node arguments.
    type THCallbackArity name :: Nat

    -- |Runs the callback for an appropriate 'Vec' of atoms
    runTHCallback :: (Monad m, ReadRepresentation m repr)
        => repr -> Vec (THCallbackArity name) Atom -> m (THCallback name)

type role THCallback nominal
-- |A wrapper around the callback return type, which will be provided an instance
-- of 'Callback'.
newtype THCallback (name :: TL.Symbol) = THCallback { getTHCallback :: THCallbackResult name }

deriving instance Eq (THCallbackResult name) => Eq (THCallback name)
deriving instance Ord (THCallbackResult name) => Ord (THCallback name)
deriving instance Num (THCallbackResult name) => Num (THCallback name)
deriving instance Fractional (THCallbackResult name) => Fractional (THCallback name)
deriving instance Enum (THCallbackResult name) => Enum (THCallback name)
deriving instance Real (THCallbackResult name) => Real (THCallback name)
deriving instance Integral (THCallbackResult name) => Integral (THCallback name)
deriving instance Read (THCallbackResult name) => Read (THCallback name)

instance Show (THCallbackResult name) => Show (THCallback name) where
    show = show . getTHCallback

instance CallbackSerialisable (THCallbackResult name) => CallbackSerialisable (THCallback name) where
    serialiseCallback s = (serialiseCallback s) . getTHCallback
    prettyPrintCallback = prettyPrintCallback . getTHCallback


-- |An auxiliary class used for lifting types into type expressions.
class LiftProxy a where
    liftProxy :: Proxy# a -> TypeQ

instance LiftProxy Int where
    liftProxy _ = [t| Int |]

instance LiftProxy Double where
    liftProxy _ = [t| Double |]

instance LiftProxy Bool where
    liftProxy _ = [t| Bool |]

instance LiftProxy 'Zero where
    liftProxy _ = [t| 'Zero |]

instance LiftProxy n => LiftProxy ('Succ n) where
    liftProxy _ = [t| 'Succ $(liftProxy (proxy# :: Proxy# n)) |]

-- |Convenient alias.
type LiftsA = Both Lift LiftProxy

-- |Convenient alias.
type LiftsN = Both ForEachKNodes LiftProxy

-- |Creates Template Haskell declarations from a suitable 'ParsedCallback'.
createCallback :: forall n a. (ForEachKNodes n, LiftProxy a, LiftProxy n)
    => ParsedCallback LiftsA n a -> Q [Dec]
createCallback ParsedCallback{..} = concat <$> sequence [common, frequency, monoid, callback]
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

    common = [d|
        instance IsTHCallback $(name) where
            type THCallbackArity $(name) = $(liftProxy (proxy# :: Proxy# n))
            type THCallbackResult $(name) = $(constrT $ liftProxy (proxy# :: Proxy# a))

            runTHCallback repr args = pure $
                if $(unTypeQ $ ev callbackCondition) then
                    THCallback $(constrE $ unTypeQ $ ev expr)
                else
                    mempty
            {-# INLINE runTHCallback #-}
        |]

    (presentedName, frequency) = case callbackFrequency of
        EveryNAcceptedFrames 1 -> (callbackName, pure [])
        EveryNAcceptedFrames n -> ('_' : callbackName, [d|
            type instance CallbackPeriod (THCallback $(name)) = $(litT . numTyLit $ fromIntegral n)
            |])
        EveryNFrames _ -> error "EveryNFrames is not supported"

    monoid = case callbackResult of
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

    callback = case callbackResult of
        CallbackSum _ -> [d|
            instance Callback 'Post (THCallback $(name)) where
                callbackName _ = $(lift presentedName)

                runCallback = forEachKNodes <*> runTHCallback
                {-# INLINE runCallback #-}

                updateCallback repr prev (MoveFromTo from to) = do
                    atom <- fromMaybe (error "No atom found. The representation is broken")
                            <$> getAtomAt to repr
                    let prevAtom = atom & position .~ from
                    fmap (prev +) $ forEachKNodesContaining atom repr $ \vec ->
                        let prevVec = replaceAll atom prevAtom vec
                            go = runTHCallback repr in
                        (-) <$> go vec <*> go prevVec
                {-# INLINEABLE updateCallback #-}
            |]
        _ -> [d|
            instance Callback 'Post (THCallback $(name)) where
                callbackName _ = $(lift callbackName)

                runCallback = forEachKNodes <*> runTHCallback
                {-# INLINE runCallback #-}
            |]

-- |A callback quasiquoter, accepting any suitable callback with arity strictly less than 'maxn'.
quoteCallback :: MaxNConstraint LiftsA LiftsN maxn => Proxy# maxn -> String -> Q [Dec]
quoteCallback p str =
    case P.parse (parseCallback p <* P.eof) "TH" str of
        Right ((ParsedCallbackWrapper exp) :: ParsedCallbackWrapper LiftsA LiftsN) ->
            createCallback exp
        Left err -> fail $ show err

type ArityLimit = ToNat 10

-- |A callback quasiquoter
callback :: QuasiQuoter
callback = QuasiQuoter
    { quoteDec = quoteCallback (proxy# :: Proxy# ArityLimit)
    }

-- |An external callbacks file quasiquoter
callbacksFile :: QuasiQuoter
callbacksFile = QuasiQuoter{..}
  where
    quoteDec fileName = do
        let parser = P.many1 (parseCallback (proxy# :: Proxy# ArityLimit)) <* P.eof
        result <- runIO $ P.parseFromFile parser fileName
        addDependentFile fileName
        case result of
            Left err -> fail $ show err
            Right (cbs :: [ParsedCallbackWrapper LiftsA LiftsN]) ->
                fold <$> sequence [createCallback exp | ParsedCallbackWrapper exp <- cbs]

-- |A fixed-width vector
data Vec (n :: Nat) a where
    Nil :: Vec 'Zero a
    (:::) :: a -> Vec n a -> Vec ('Succ n) a

infixr 5 :::

-- |Evaluation context
data EvalCtx n = EvalCtx
    { evalCtxArgs :: Q (TExp (Vec n Atom)) -- ^A typed expression representing the arguments vector
    , evalCtxRepr :: Name -- ^The name of the binding of the representation
    }

-- |Convert 'TL.Nat' to 'Nat'.
type family ToNat (n :: TL.Nat) :: Nat where
    ToNat 0 = 'Zero
    ToNat n = 'Succ (ToNat (n TL.- 1))

-- |Type-safe !! on 'Vec'tors.
access :: Node n -> Vec n a -> a
access FirstNode (h ::: _) = h
access (NextNode n) (_ ::: t) = access n t
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
        (AtomTypeBeadBindingTo x, Bead b) -> energyBetween (b ^. beadEV) x > 0
        (AtomTypeBinder x, Binder b) -> x == b ^. binderType
        (AtomTypeAnyBead, Bead _) -> True
        (AtomTypeAnyBinder, Binder _) -> True
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
        Bead b -> b ^. beadAtomIndex
        _ -> -1
    ||]
eval EvalCtx{..} (EChainIx node) = [||
    case access node $$(evalCtxArgs) of
        Bead b -> b ^. beadChain
        _ -> -1
    ||]
eval EvalCtx{..} (EChromoIx node) = [||
    case access node $$(evalCtxArgs) of
        Bead b -> b ^. beadIndexOnChain
        _ -> -1
    ||]
eval EvalCtx{..} (EEnergy lhs rhs) = [||
    energyBetween
        (access lhs $$(evalCtxArgs))
        (access rhs $$(evalCtxArgs))
    ||]

instance Lift AtomType where
    lift (AtomTypeBeadBindingTo cls)   = [| AtomTypeBeadBindingTo cls |]
    lift (AtomTypeBinder cls) = [| AtomTypeBinder cls |]
    lift AtomTypeAnyBead = [| AtomTypeAnyBead |]
    lift AtomTypeAnyBinder = [| AtomTypeAnyBinder |]

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

    forEachKNodesContaining :: (Monoid r, ReadRepresentation m repr, Monad m)
        => Atom -> repr -> (Vec ('Succ n) Atom -> m r) -> m r

-- |The base case.
instance ForEachKNodes 'Zero where
    forEachKNodes _ fun = fun Nil
    {-# INLINE forEachKNodes #-}

    forEachKNodesContaining atom _ fun = fun $ atom ::: Nil
    {-# INLINE forEachKNodesContaining #-}

-- |The recursive case.
instance ForEachKNodes n => ForEachKNodes ('Succ n) where
    forEachKNodes repr fun = forEachNode repr $ \x ->
        forEachKNodes repr $ fun . (x :::)
    {-# INLINE forEachKNodes #-}

    forEachKNodesContaining atom repr fun = mappend <$> inHead <*> inTail
      where
        inHead = forEachKNodes repr $ fun . (atom :::)
        inTail = forEachNode repr $ \x ->
            if x == atom then
                pure mempty
            else
                forEachKNodesContaining atom repr $ fun . (x :::)
    {-# INLINE forEachKNodesContaining #-}

-- |Performs a monadic action over all nodes (i.e. beads and atoms)
-- and gathers the results monoidally.
forEachNode :: forall m r repr. (Monoid r, ReadRepresentation m repr, Monad m)
    => repr -> (Atom -> m r) -> m r
forEachNode repr f = do
    numChains <- getNumberOfChains repr
    binders <- getBinders repr go
    beads <- fold <$> traverse (\idx -> getChain repr idx go) [0..numChains - 1]
    pure $ beads <> binders
  where
    go :: (MonoTraversable c, Element c ~ a, AsAtom a) => c -> m r
    go = flip ofoldlM mempty $ \s x -> mappend s <$> f (asAtom x)
{-# INLINE forEachNode #-}

replaceAll :: Eq a => a -> a -> Vec n a -> Vec n a
replaceAll _ _ Nil = Nil
replaceAll from to (x ::: xs)
    | from == x = to ::: rest
    | otherwise = x ::: rest
    where rest = replaceAll from to xs
