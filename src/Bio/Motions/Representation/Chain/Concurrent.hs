{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Bio.Motions.Representation.Chain.Concurrent where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Class
import Bio.Motions.Representation.Dump
import Bio.Motions.Representation.Chain.Internal
import Bio.Motions.Representation.Common
import Bio.Motions.Utils.Random

import Linear
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Lens hiding (ix, from, to)
import Data.IORef
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Array as A
import qualified System.Random.MWC as MWC
import qualified Data.HashMap.Strict as M

data Request = Perform Move
             | Generate

data Cube = Cube
    { cubeChan :: Chan Request
    , cubeMVar :: MVar (Maybe Move)
    , cubeLow :: Vec3
    , cubeHigh :: Vec3
    , cubeRepr :: IORef PureChainRepresentation
    , cubeLocalBeads :: IORef (S.Set Int)
    , cubeLocalBinders :: IORef (S.Set Int)
    }

data ConcurrentChainRepresentation = ConcurrentChainRepresentation
    { repr :: IORef PureChainRepresentation
    , cubes :: A.Array Vec3 Cube
    , cubeSideLen :: Int
    }

instance MonadIO m => Representation m ConcurrentChainRepresentation where
    type ReprRandomTypes m ConcurrentChainRepresentation = '[Int, Bool]

    loadDump dump isFrozen = do
        let cubeSideLen = sideLen -- TODO
        repr <- loadDump dump isFrozen >>= liftIO . newIORef
        cubes <- liftIO $ mapM (uncurry $ initCube repr) cubeBounds
        pure ConcurrentChainRepresentation{..}
      where
        cubeBounds = initCubeBounds (lowPos dump) (highPos dump) sideLen
        sideLen = 10

    makeDump = liftIO . readIORef . repr >=> makeDump

    generateMove r@ConcurrentChainRepresentation{..} = do
        repr' <- liftIO $ readIORef repr
        moveBinder <- getRandom
        pos <- if moveBinder then do
                   ix <- getRandomElement (moveableBinders repr')
                   pure $ (V.unsafeIndex (binders repr') ix) ^. position
               else do
                   ix <- getRandomElement (moveableBeads repr')
                   pure $ (V.unsafeIndex (beads repr') ix) ^. position
        let cube = cubeAt r pos
        liftIO $ do
            writeChan (cubeChan cube) Generate
            takeMVar $ cubeMVar cube
    {-# INLINEABLE generateMove #-}

    performMove move r@ConcurrentChainRepresentation{..} = do
        repr' <- liftIO $ readIORef repr
        (repr'', _) <- performMove move repr'
        liftIO $ atomicWriteIORef repr repr''
        liftIO $ forM_ (closeCubes r move) $ \cube -> writeChan (cubeChan cube) (Perform move)
        pure (r, [])
    {-# INLINEABLE performMove #-}

instance MonadIO m => ReadRepresentation m ConcurrentChainRepresentation where
    getBinders r f = liftIO (readIORef $ repr r) >>= (`getBinders` f)
    {-# INLINE getBinders #-}
    getNumberOfChains = liftIO . readIORef . repr >=> getNumberOfChains
    {-# INLINE getNumberOfChains #-}
    getChain r k f = liftIO (readIORef $ repr r) >>= (\r' -> getChain r' k f)
    {-# INLINE getChain #-}
    getAtomAt pos = liftIO . readIORef . repr >=> getAtomAt pos
    {-# INLINE getAtomAt #-}

lowPos :: Dump -> Vec3
lowPos Dump{..} = foldr1 (liftA2 min) allPositions
  where
    allPositions = map dumpBeadPosition (concat dumpChains) ++ map (^. position) dumpBinders

highPos :: Dump -> Vec3
highPos Dump{..} = foldr1 (liftA2 max) allPositions
  where
    allPositions = map dumpBeadPosition (concat dumpChains) ++ map (^. position) dumpBinders

initCube
  :: IORef PureChainRepresentation -- ^The representation
  -> Vec3 -- ^Low
  -> Vec3 -- ^High
  -> IO Cube
initCube cubeRepr cubeLow cubeHigh = do
    cubeMVar <- newEmptyMVar
    cubeChan <- newChan
    cubeLocalBeads <- initLocalBeads
    cubeLocalBinders <- initLocalBinders
    let cube = Cube{..}
    _ <- forkIO $ cubeWorker cube
    pure cube
  where
    initLocalBeads :: IO (IORef (S.Set Int))
    initLocalBeads = readIORef cubeRepr >>= \repr -> newIORef
        $ S.fromList [ix | ix <- U.toList (moveableBeads repr),
                           insideThisCube ((beads repr V.! ix) ^. position)]

    initLocalBinders :: IO (IORef (S.Set Int))
    initLocalBinders = readIORef cubeRepr >>= \repr -> newIORef
        $ S.fromList [ix | ix <- U.toList (moveableBinders repr),
                           insideThisCube ((binders repr V.! ix) ^. position)]

    insideThisCube :: Vec3 -> Bool
    insideThisCube = insideCube cubeLow cubeHigh

cubeWorker :: Cube -> IO ()
cubeWorker Cube{..} = do
    gen <- MWC.createSystemRandom
    forever $ do
        repr <- readIORef cubeRepr
        move <- do
            localBinders <- readIORef cubeLocalBinders
            localBeads <- readIORef cubeLocalBeads
            moveBinder <- MWC.uniform gen
            if moveBinder && (not . S.null) localBinders then do
                ix <- (`S.elemAt` localBinders) <$> MWC.uniformR (0, S.size localBinders - 1) gen
                x <- V.indexM (binders repr) ix
                d <- V.unsafeIndex legalMoves <$> MWC.uniformR (0, V.length legalMoves - 1) gen
                let pos = x ^. position
                    pos' = pos + d
                runMaybeT $ guard (not $ M.member pos' (space repr)) >> pure (Move pos d)
            else if (not . S.null) localBeads then do
                ix <- (`S.elemAt` localBeads) <$> MWC.uniformR (0, S.size localBeads - 1) gen
                x <- V.indexM (beads repr) ix
                d <- V.unsafeIndex legalMoves <$> MWC.uniformR (0, V.length legalMoves - 1) gen
                let pos = x ^. position
                    pos' = pos + d
                runMaybeT $ do
                    guard . not $ M.member pos' $ space repr
                    let m = Move pos d
                    illegalBeadMove repr m x >>= guard . not
                    pure m
            else
                pure Nothing
        reply repr move
  where
    reply :: PureChainRepresentation -> Maybe Move -> IO ()
    reply oldRepr !move = readChan cubeChan >>= \case
        Generate -> do
            putMVar cubeMVar move
        Perform move'@(MoveFromTo from to) -> do
            when (entersCube move') $ do
                newRepr <- readIORef cubeRepr
                addAtom newRepr to
            when (leavesCube move') $ removeAtom oldRepr from
            --case move of
            --  Just m -> unless (isClose m move') $ reply oldRepr move
            --  Nothing -> pure ()

    addAtom :: PureChainRepresentation -> Vec3 -> IO ()
    addAtom = updateAtom S.insert

    removeAtom :: PureChainRepresentation -> Vec3 -> IO ()
    removeAtom = updateAtom S.delete

    updateAtom :: (Int -> S.Set Int -> S.Set Int) -> PureChainRepresentation -> Vec3 -> IO ()
    updateAtom update repr pos =
        case [beadIndex repr pos, binderIndex repr pos] of
            [Nothing, Nothing] -> error "That's bad"
            [Just _, Just _] -> error "That's even worse"
            [Just ix, _] -> modifyIORef' cubeLocalBeads $ update ix
            [_, Just ix] -> modifyIORef' cubeLocalBinders $ update ix
            _ -> error "wtf"

    -- TODO: slow
    beadIndex :: PureChainRepresentation -> Vec3 -> Maybe Int
    beadIndex repr pos = V.findIndex ((== pos) . (^. position)) . beads $ repr

    binderIndex :: PureChainRepresentation -> Vec3 -> Maybe Int
    binderIndex repr pos = V.findIndex ((== pos) . (^. position)) . binders $ repr

    entersCube :: Move -> Bool
    entersCube (MoveFromTo from to) = not (insideThisCube from) && insideThisCube to

    leavesCube :: Move -> Bool
    leavesCube (MoveFromTo from to) = insideThisCube from && not (insideThisCube to)

    -- assuming move qd <= 2 TODO
    isClose :: Move -> Move -> Bool
    isClose (MoveFromTo from to) (MoveFromTo from' to') = any (<= 2) (qd <$> [from, to] <*> [from', to'])

    insideThisCube :: Vec3 -> Bool
    insideThisCube = insideCube cubeLow cubeHigh

insideCube :: Vec3 -> Vec3 -> Vec3 -> Bool
insideCube cubeLow cubeHigh v = and (between <$> cubeLow <*> cubeHigh <*> v)
  where
    between :: Int -> Int -> Int -> Bool
    between a b x = a <= x && x <= b

initCubeBounds
  :: Vec3 -- ^Low
  -> Vec3 -- ^High
  -> Int -- ^Cube side length
  -> A.Array Vec3 (Vec3, Vec3) -- ^Array of (low, high) pairs
initCubeBounds low high sideLen = A.array bounds list
  where
    bounds = (fmap (`div` sideLen) low, fmap (`div` sideLen) high)
    list = [(V3 i j k, (l, h)) | V3 i j k <- A.range bounds,
                                 let l = V3 i j k ^* sideLen,
                                 let h = V3 (i + 1) (j + 1) (k + 1) ^* sideLen - V3 1 1 1]

cubeAt :: ConcurrentChainRepresentation -> Vec3 -> Cube
cubeAt r = (cubes r A.!) . fmap (`div` cubeSideLen r)

-- assuming qd <= 3; TODO
closeCubes :: ConcurrentChainRepresentation -> Move -> [Cube]
closeCubes ConcurrentChainRepresentation{..} (MoveFromTo from to) =
    let V3 lx ly lz = fmap (`div` cubeSideLen) ((min <$> from <*> to) - V3 1 1 1)
        V3 hx hy hz = fmap (`div` cubeSideLen) ((max <$> from <*> to) + V3 1 1 1)
    in [cubes A.! V3 i j k | i <- [lx..hx], j <- [ly..hy], k <- [lz..hz]]
{-# INLINE closeCubes #-}
