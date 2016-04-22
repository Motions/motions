{- |
Module      : Bio.Motions.Representation.Chain.Internal
Description : Contains the internal definitions for the 'Pure Chain Representation'.
License     : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Bio.Motions.Representation.Chain.Internal where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Random
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List
import Data.IORef
import Data.Maybe
import Data.MonoTraversable
import qualified Data.Sequences as DS
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Linear

data ChainRepresentation f = ChainRepresentation
    { space :: !(Space' f)
    , binders :: !(V.Vector (BinderInfo' f))
    , moveableBinders :: !(U.Vector Int)
    -- ^ Indices (in the 'binders' vector) of the binders that may be moved.
    , beads :: !(V.Vector (BeadInfo' f))
    -- ^ Beads from all chains
    , moveableBeads :: !(U.Vector Int)
    -- ^ Indices (in the 'beads' vector) of the beads that may be moved.
    , chainIndices :: !(U.Vector Int)
    -- ^ Indices of first atoms of successive chains in the 'beads' vector,
    -- with an additional @'V.length' 'beads'@ at the end -- see 'getChain''.
    }

type PureChainRepresentation = ChainRepresentation Identity
type IOChainRepresentation = ChainRepresentation IORef

-- |Used to wrap and unwrap 'f'.
--
-- See 'relocate'.
class Monad m => Wrapper m f where
    unwrap :: f a -> m a
    wrap :: a -> m (f a)

instance Monad m => Wrapper m Identity where
    unwrap = pure . runIdentity
    {-# INLINE unwrap #-}
    wrap = pure . Identity
    {-# INLINE wrap #-}

instance MonadIO m => Wrapper m IORef where
    unwrap = liftIO . readIORef
    {-# INLINE unwrap #-}
    wrap = liftIO . newIORef
    {-# INLINE wrap #-}

-- |Converts between 'Located f' and 'Located f''.
relocate :: (Wrapper m f, Wrapper m f') => Located' f a -> m (Located' f' a)
relocate (Located' p a) = unwrap p >>= fmap (flip Located' a) . wrap
{-# INLINE[2] relocate #-}
{-# RULES "relocate/pure" relocate = pure #-}

-- |A type-constrained version of 'relocate'.
retrieveLocated :: Wrapper m f => Located' f a -> m (Located a)
retrieveLocated = relocate
{-# INLINE[2] retrieveLocated #-}
{-# RULES "retrieveLocated/pure" retrieveLocated = pure #-}

instance Wrapper m f => ReadRepresentation m (ChainRepresentation f) where
    getBinders ChainRepresentation{..} f = mapM retrieveLocated binders >>= f
    {-# INLINE getBinders #-}

    getNumberOfChains ChainRepresentation{..} = pure $ U.length chainIndices - 1
    {-# INLINE getNumberOfChains #-}

    getChain repr ix f = mapM retrieveLocated (getChain' repr ix) >>= f
    {-# INLINE getChain #-}

    getAtomAt pos ChainRepresentation{..} = pure $ Located pos . (^. located) <$> M.lookup pos space
    {-# INLINE getAtomAt #-}

instance Monad m => Representation m PureChainRepresentation where
    type ReprRandomTypes m PureChainRepresentation = '[Int, Bool]

    loadDump = loadDump'
    makeDump = makeDump'
    generateMove = generateMove'
    {-# INLINEABLE generateMove #-}

    performMove (MoveFromTo from to) repr
        | Binder binderSig <- atom = pure $
            let Just idx = V.elemIndex (Located from binderSig) $ binders repr
            in  (repr { space = space'
                      , binders = binders repr V.// [(idx, Located to binderSig)]
                      }, [])
        | Bead beadSig <- atom = pure
            (repr { space = space'
                  , beads = beads repr V.// [(beadSig ^. beadAtomIndex, Located to beadSig)]
                  }, [])
      where
        atom = space repr M.! from
        space' = M.insert to (atom & position .~ to) . M.delete from $ space repr
    {-# INLINEABLE performMove #-}

instance MonadIO m => Representation m IOChainRepresentation where
    type ReprRandomTypes m IOChainRepresentation = '[Int, Bool]

    loadDump = loadDump'
    makeDump = makeDump'
    generateMove = generateMove'
    {-# INLINEABLE generateMove #-}

    performMove (MoveFromTo from to) repr = do
        liftIO $ writeIORef (atom ^. wrappedPosition) to
        pure (repr { space = space' }, [])
      where
        atom = space repr M.! from
        space' = M.insert to atom $ M.delete from $ space repr
    {-# INLINEABLE performMove #-}

-- |An 'f'-polymorphic implementation of 'loadDump' for 'ChainRepresentation f'.
loadDump' :: _ => Dump -> FreezePredicate -> m (ChainRepresentation f)
loadDump' Dump{..} isFrozen = do
    relBinders <- mapM relocate dumpBinders
    relBeads <- mapM relocate (concat chains)
    let moveableBinders = U.fromList [i | (i, b) <- zip [0..] relBinders, b ^. binderType /= laminType]
        moveableBeads = U.fromList [i | (i, b) <- zip [0..] relBeads, not . isFrozen $ b ^. beadSignature]
    when (U.null moveableBinders) $ fail "No moveable binders"
    when (U.null moveableBeads) $ fail "No moveable beads"
    pure ChainRepresentation
        { binders = V.fromList relBinders
        , moveableBinders = moveableBinders
        , beads = V.fromList relBeads
        , moveableBeads = moveableBeads
        , chainIndices = U.fromList . scanl' (+) 0 $ map length chains
        , space = M.fromList $ zipWith convert relBinders dumpBinders ++ zipWith convert relBeads (concat chains)
        }
  where
    chains = addIndices dumpChains
    convert new old = (old ^. position, asAtom' new)

-- |An 'f'-polymorphic implementation of 'makeDump' for 'ChainRepresentation f'.
makeDump' :: _ => ChainRepresentation f -> m Dump
makeDump' repr = do
    relBinders <- mapM relocate (V.toList $ binders repr)
    relChains <- mapM (mapM relocate . V.toList . getChain' repr) [0..U.length (chainIndices repr) - 2]
    pure Dump
        { dumpBinders = relBinders
        , dumpChains = (dropIndices <$>) <$> relChains
        }

-- |An 'f'-polymorphic implementation of 'generateMive' for 'ChainRepresentation f'.
generateMove' :: _ => ChainRepresentation f -> m (Maybe Move)
generateMove' repr@ChainRepresentation{..} = do
    moveBinder <- getRandom
    if moveBinder then
        pick moveableBinders binders []
    else
        pick moveableBeads beads [illegalBeadMove repr]
  where
    -- |Pick a random move of some atom in a sequence
    pick :: _  -- Under some cumbersome constraints...
        => ixs -- ^The sequence of moveable atoms' indices
        -> s -- ^The sequence of atoms
        -> t (Move -> Element s -> m Bool) -- ^A 'Traversable' of additional move constraints
        -> m (Maybe Move)
    pick ixs xs constraints = do
        ix <- getRandomElement ixs
        let x = DS.unsafeIndex xs ix
        d <- getRandomElement legalMoves
        r <- retrieveLocated x
        let pos = r ^. position
            pos' = pos + d
        runMaybeT $ do
            guard . not $ M.member pos' space
            let m = Move pos d
            forM_ constraints $ \c -> lift (c m x) >>= guard . not
            pure m
    {-# INLINE pick #-}
{-# INLINE generateMove' #-}

-- |Picks a random element from a 'DS.IsSequence', assuming that its indices form
-- a continuous range from 0 to @'olength' s - 1@.
getRandomElement :: (Generates '[Int] m, DS.IsSequence s, DS.Index s ~ Int) => s -> m (Element s)
getRandomElement s = DS.unsafeIndex s <$> {-# SCC "getRandomR" #-} (getRandomR (0, olength s - 1))
{-# INLINE getRandomElement #-}

-- |The pairs of local neighbours of a bead
localNeighbours :: (Wrapper m f, Wrapper m f')
    => BeadInfo' f -> ChainRepresentation f' -> m [(Vec3, Vec3)]
localNeighbours info repr = do
    neighbours <- sequence $ catMaybes
          [ fmap retrieveLocated  $  chain V.!? (ix - 1)
          ,      retrieveLocated <$> Just info
          , fmap retrieveLocated  $  chain V.!? (ix + 1)
          ]
    let positions = view position <$> neighbours
    pure $ zip positions (tail positions)
  where
    ix = info ^. beadIndexOnChain
    chain = getChain' repr $ info ^. beadChain
{-# INLINE localNeighbours #-}

illegalBeadMove :: Wrapper m f => ChainRepresentation f -> Move -> BeadInfo' f -> m Bool
illegalBeadMove repr Move{..} bead = do
    bead' <- retrieveLocated bead
    pairs <- localNeighbours (bead' & position +~ moveDiff) repr
    pure $ any (uncurry notOk) pairs
  where
    notOk b1 b2 = wrongQd (qd b1 b2) || intersectsChain (space repr) b1 b2
    wrongQd d = d <= 0 || d > 2
{-# INLINE illegalBeadMove #-}

-- |Returns the chain with the specified index.
getChain' :: ChainRepresentation f -> Int -> V.Vector (BeadInfo' f)
getChain' ChainRepresentation{..} ix = V.slice b (e - b) beads
  where
    [b, e] = U.unsafeIndex chainIndices <$> [ix, ix + 1]
{-# INLINE getChain' #-}
