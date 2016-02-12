{- |
Module      : Bio.Motions.Representation.Chain.Internal
Description : Contains the internal definitions for the 'Pure Chain Representation'.
License     : MIT
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Bio.Motions.Representation.Chain.Internal where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.Dump
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.List
import Data.IORef
import Data.Maybe
import Data.MonoTraversable
import qualified Data.Sequences as DS
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Linear

type Space f = M.Map Vec3 (Atom' f)

data ChainRepresentation f = ChainRepresentation
    { space :: !(Space f)
    , binders :: !(V.Vector (BinderInfo' f))
    , beads :: !(V.Vector (BeadInfo' f))
    -- ^ Beads from all chains
    , chainIndices :: !(U.Vector Int)
    -- ^ Indices of first atoms of successive chains in the 'beads' vector,
    -- with an additional @'V.length' 'beads'@ at the end -- see 'getChain''.
    , radius :: !Int
    -- ^ Radius of the bounding sphere
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

-- |A type-constrained version of 'reloate'
retrieveLocated :: Wrapper m f => Located' f a -> m (Located a)
retrieveLocated = relocate

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
    loadDump = loadDump'
    makeDump = makeDump'
    generateMove = generateMove'

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

instance MonadIO m => Representation m IOChainRepresentation where
    loadDump = loadDump'
    makeDump = makeDump'
    generateMove = generateMove'

    performMove (MoveFromTo from to) repr = do
        liftIO $ writeIORef (atom ^. location) to
        pure (repr { space = space' }, [])
      where
        atom = space repr M.! from
        space' = M.insert to atom $ M.delete from $ space repr

-- |An 'f'-polymorphic implementation of 'loadDump' for 'ChainRepresentation f'.
loadDump' :: _ => Dump -> m (ChainRepresentation f)
loadDump' Dump{..} = do
    relBinders <- mapM relocate dumpBinders
    relBeads <- mapM relocate (concat chains)
    pure ChainRepresentation
        { binders = V.fromList relBinders
        , beads = V.fromList relBeads
        , chainIndices = U.fromList . scanl' (+) 0 $ map length chains
        , space = M.fromList $ zipWith convert relBinders dumpBinders ++ zipWith convert relBeads (concat chains)
        , radius = dumpRadius
        }
  where
    chains = addIndices dumpChains
    convert new old = (old ^. position, asAtom new)

-- |An 'f'-polymorphic implementation of 'makeDump' for 'ChainRepresentation f'.
makeDump' :: _ => ChainRepresentation f -> m Dump
makeDump' repr = do
    relBinders <- mapM relocate (V.toList $ binders repr)
    relChains <- mapM (mapM relocate . V.toList . getChain' repr) [0..U.length (chainIndices repr) - 2]
    pure Dump
        { dumpBinders = relBinders
        , dumpChains = (dropIndices <$>) <$> relChains
        , dumpRadius = radius repr
        }

-- |An 'f'-polymorphic implementation of 'generateMive' for 'ChainRepresentation f'.
generateMove' :: _ => ChainRepresentation f -> m Move
generateMove' repr@ChainRepresentation{..} = do
    moveBinder <- getRandom
    if moveBinder then
        pick binders Nothing
    else
        pick beads $ Just $ illegalBeadMove repr
  where
    -- |Pick a random move of some atom in a sequence
    pick :: _  -- Under some cumbersome constraints...
        => s -- ^The sequence of atoms
        -> t (Move -> Element s -> m Bool) -- ^A 'Traversable' of additional move constraints
        -> m Move
    pick xs constraints = do
        x <- getRandomElement xs
        d <- getRandomElement legalMoves
        r <- retrieveLocated x
        let pos = r ^. position
            pos' = pos + d
        guard $ not $ M.member pos' space
        let m = Move pos d
        forM_ constraints $ \c -> c m x >>= guard . not
        pure m

-- |Picks a random element from a 'DS.IsSequence', assuming that its indices form
-- a continuous range from 0 to @'olength' s - 1@.
getRandomElement :: (MonadRandom m, DS.IsSequence s, DS.Index s ~ Int) => s -> m (Element s)
getRandomElement s = DS.unsafeIndex s <$> getRandomR (0, olength s - 1)

-- |The legal moves an atom may make
legalMoves :: V.Vector Vec3
legalMoves = V.fromList [v | [x, y, z] <- replicateM 3 [-1, 0, 1],
                             let v = V3 x y z,
                             quadrance v `elem` [1, 2]]

-- |The pairs of local neighbours of a bead
localNeighbours :: (Wrapper m f, Wrapper m f')
    => BeadInfo' f -> ChainRepresentation f' -> m [(Vec3, Vec3)]
localNeighbours info repr = do
    neighbours <- sequence $ catMaybes
          [ fmap retrieveLocated  .  DS.index chain $ ix - 1
          ,      retrieveLocated <$> Just info
          , fmap retrieveLocated  .  DS.index chain $ ix + 1
          ]
    let positions = view position <$> neighbours
    pure $ zip positions (tail positions)
  where
    ix = info ^. beadIndexOnChain
    chain = getChain' repr $ info ^. beadChain

-- |Checks if a segment connecting the two given points would intersect with a chain.
-- Assumes that these points are neighbours on the 3-dimensional grid, i. e. the quadrance
-- of the distance between these points equals 1 or 2.
intersectsChain :: Space f -> Vec3 -> Vec3 -> Bool
intersectsChain space v1@(V3 x1 y1 z1) v2@(V3 x2 y2 z2) =
    d /= 1 && case (`M.lookup` space) <$> crossPoss of
                [Just (Bead b1), Just (Bead b2)] -> chainNeighbours b1 b2
                _                                      -> False
  where
    d = qd v1 v2
    crossPoss | x1 == x2 = [V3 x1 y1 z2, V3 x1 y2 z1]
              | y1 == y2 = [V3 x1 y1 z2, V3 x2 y1 z1]
              | z1 == z2 = [V3 x1 y2 z1, V3 x2 y1 z1]
    chainNeighbours b1 b2 = b1 ^. beadChain == b2 ^. beadChain
                         && abs (b1 ^. beadIndexOnChain - b2 ^. beadIndexOnChain) == 1

illegalBeadMove :: Wrapper m f => ChainRepresentation f -> Move -> BeadInfo' f -> m Bool
illegalBeadMove repr Move{..} bead = do
    bead' <- retrieveLocated bead
    pairs <- localNeighbours (bead' & position +~ moveDiff) repr
    pure $ any (uncurry notOk) pairs
  where
    notOk b1 b2 = wrongQd (qd b1 b2) || intersectsChain (space repr) b1 b2
    wrongQd d = d <= 0 || d > 2

-- |Returns the chain with the specified index.
getChain' :: ChainRepresentation f -> Int -> V.Vector (BeadInfo' f)
getChain' ChainRepresentation{..} ix = V.slice b (e - b) beads
  where
    [b, e] = U.unsafeIndex chainIndices <$> [ix, ix + 1]
