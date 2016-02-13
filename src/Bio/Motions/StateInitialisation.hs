{- |
Module      : Bio.Motions.StateInitialisation
Description : Random initiation of state for new simulation.
License     : MIT
Stability   : experimental
Portability : unportable
-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.StateInitialisation where

import Control.Lens
import Control.Monad.Random
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import GHC.Exts(groupWith, sortWith)
import Linear
import System.Random.Shuffle

import Bio.Motions.Common
import Bio.Motions.Types
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.RepetitionGuard

-- |Creates an initial state
initialize :: (MonadRandom m) =>
     Int
  -- ^Maximum number of initialization attempts
  -> Int
  -- ^Radius
  -> Int
  -- ^Number of binders
  -> Int
  -- ^Number of binders' types
  -> [[EnergyVector]]
  -- ^EnergyVectors of beads
  -> m (Maybe Dump)
  -- ^Initial state
initialize maxTries r bindersCount binderTypesCount evs = runRepetitionGuardT maxTries $
    spaceToDump r <$> initializeSpace r bindersCount binderTypesCount evs

-- |Creates an initial state in a form of Space
initializeSpace :: (MonadRandom m) =>
     Int
  -- ^Radius
  -> Int
  -- ^Number of binders
  -> Int
  -- ^Number of binders' types
  -> [[EnergyVector]]
  -- ^EnergyVectors of beads
  -> RepetitionGuardT m Space
  -- ^Initial state
initializeSpace r bindersCount bindersTypesCount beads = do
    stateWithChains <- placeChains r stateWithLamins beads
    placeBinders stateWithChains
      where
        stateWithLamins = addLamins M.empty $ spherePoints (fromIntegral r)
        placeBinders space = foldM (\s _ -> addBinder r bindersTypesCount s) space [1..bindersCount]

-- |Converts Space to Dump
spaceToDump :: Int -> Space -> Dump
spaceToDump dumpRadius space = Dump{..}
  where
    dumpBinders = [Located l b | Located l (BinderSig b) <- M.elems space]
    beads = [Located l b | Located l (BeadSig b) <- M.elems space]
    dumpChains = map (map dropIndices) $
        groupWith (view beadChain) $ sortWith (view beadAtomIndex) beads

-- |Generates all points, such that their distance from (0,0,0)
-- is in the range [radius - 2, radius + 2]
spherePoints :: Int -> [Vec3]
spherePoints r = [v | [x,y,z] <- replicateM 3 [-r..r],
                      let v = V3 x y z,
                      let len = quadrance v,
                      len >= (r - 2)^2 && len <= (r + 2)^2]

-- |Adds lamin binders into space
addLamins :: Space -> [Vec3] -> Space
addLamins space positions =
    space `M.union` M.fromList [(v3, asAtom $ BinderInfo v3 $ laminType) | v3 <- positions]

-- |Tries to place beads of one chain in the space
addChain :: (MonadRandom m) =>
    Int
 -- ^Index of the chain we are placing
 -> Int
 -- ^Chain index of the first bead to be placed
 -> Int
 -- ^Global index of the first atom to be placed
 -> Vec3
 -- ^Starting point
 -> Space
 -- ^Current space
 -> [EnergyVector]
 -- ^Energy vectors of atoms, that are going to be placed
 -> RepetitionGuardT m Space
 -- ^The state with a placed chain
addChain _ _ _ _ space [] = return space
addChain chainNo indOnChain glInd start space [ev] =
    return $ M.insert start (asAtom $ BeadInfo start ev glInd chainNo indOnChain) space
addChain chainNo indOnChain glInd start space (ev:evs) = hide $ do
    nextRep
    moves <- shuffleM $ V.toList legalMoves
    let targets = [target | move <- moves,
                            let target = move + start,
                            not $ target `M.member` space,
                            not $ intersectsChain space start target]
    let newSpace = M.insert start (asAtom $ BeadInfo start ev glInd chainNo indOnChain) space
    endings <- mapM (\m ->
        catch $ addChain chainNo (indOnChain + 1) (glInd + 1) m newSpace evs) targets
    return $ listToMaybe . catMaybes $ endings

-- |Tries to find a free position
getRandomFreePosition :: (MonadRandom m) => Int -> Space -> RepetitionGuardT m Vec3
getRandomFreePosition r space = do
    nextRep
    [x, y, z] <- replicateM 3 $ getRandomR (-r, r)
    let ans = V3 x y z
    if M.member ans space || quadrance ans > r^2
        then getRandomFreePosition r space else return ans

-- |Places a binder of random type in a space
addBinder :: (MonadRandom m) =>
    Int
 -- ^Radius
 -> Int
 -- ^Number of binder types (excluding lamin),
 -- must be a non-negative integer
 -> Space
 -- ^Current space
 -> RepetitionGuardT m Space
 -- ^Space with added binder
addBinder r types space = do
    pos <- getRandomFreePosition r space
    binderType <- getRandomR (1, types)
    return $ M.insert pos (asAtom $ BinderInfo pos $ BinderType binderType) space

-- |Finds a good starting point for a chain and places it there
hookAndPlaceChain :: (MonadRandom m) =>
    Int
 -- ^Radius
 -> Int
 -- ^Chain number
 -> Int
 -- ^Global index of the first bead in the chain
 -> [EnergyVector]
 -- ^Energy vectors of beads in the chain
 -> Space
 -- ^Current space
 -> RepetitionGuardT m Space
hookAndPlaceChain r chainNo glInd vectors space = do
    start <- getRandomFreePosition r space
    result <- catch $ addChain chainNo 0 glInd start space vectors
    maybe (hookAndPlaceChain r chainNo glInd vectors space) return result

-- |Places all the chains somewhere in the space
placeChains :: (MonadRandom m) => Int -> Space -> [[EnergyVector]] -> RepetitionGuardT m Space
placeChains r space vectors =
    fst <$> foldM (\(s, count) (ev, nr) -> (,count + length ev) <$>
        hookAndPlaceChain r nr count ev s) (space, 0) (zip vectors [0..])
