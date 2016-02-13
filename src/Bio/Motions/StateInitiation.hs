{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.StateInitiation where

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

import Bio.Motions.Types
import Bio.Motions.RandomShuffle
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.RepetitionGuard



-- |Creates an initial state
initialize :: (MonadRandom m) =>
     Int
  -- ^Maximum number of initialisanton attempts
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
initialize maxTries r bind bt evs =
    fmap fst $ flip runStateT maxTries $ runMaybeT $ spaceToDump r <$> initializeSpace r bind bt evs

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
initializeSpace r bindersCount bindersTypesCount beads  = do
    stateWithChains <- placeChains r stateWithLamins beads
    foldM (\s _ -> placeBinder r bindersTypesCount s) stateWithChains [1..bindersCount]
  where
    stateWithLamins = addLamins M.empty $ spherePoints (fromIntegral r)

-- |Convers Space to Dump
spaceToDump :: Int -> Space -> Dump
spaceToDump dumpRadius space = Dump{..}
    where
        dumpBinders = [x | Binder x <- M.elems space]
        beads = [x | Bead x <- M.elems space]
        dumpChains = map (map dropIndices) $ groupWith beadChain $ sortWith beadAtomIndex beads

-- |Generates all points, such that their distance from (0,0,0)
-- is in the range [radius - 2, radius + 2]
spherePoints :: Int -> [Vec3]
spherePoints r = [v | [x,y,z] <- replicateM 3 [-r..r],
                      let v = V3 x y z,
                      let len = sqrt $ fromIntegral $ quadrance v,
                      let radius = fromIntegral r,
                      len >= (radius - 2) && len <= (radius + 2)]

-- |Adds lamin binders into space
addLamins :: Space -> [Vec3] -> Space
--addLamins = foldl (\dict v3 -> M.insert v3 (Binder BinderInfo{binderPosition=v3, binderType=BinderType 0}) dict)
addLamins space positions = space `M.union` M.fromList
                                            [(v3, Binder $ BinderInfo v3 $ BinderType 0) | v3 <- positions]

-- |Tries to place beads of one chain in the space
addChain :: (MonadRandom m) =>
    Int
 -- ^Index of the chain we are placing
 -> Int
 -- ^Index of the first atom to be placed
 -> Int
 -- ^Global index of the first atom to be placed
 -> Vec3
 -- ^Starting point
 -> Space
 -- ^Current space
 ->  [EnergyVector]
 -- ^Energy vectors of atoms, that are going to be placed
 ->  RepetitionGuardT m Space
 -- ^The state with a placed chain, or nothing if no initalization was found
addChain _ _ _ _ space [] = return space
addChain chainNo indOnChain glInd start space [ev] =
    return $ M.insert start (Bead $ BeadInfo start ev glInd chainNo indOnChain) space
addChain chainNo indOnChain glInd start space (ev:evs) = hide $ do
    nextRep
    listToMaybe . catMaybes <$> endings
        where
            -- all legal moves in random order
            moves = map (+start) <$> randomShuffleM (V.toList legalMoves)
            -- only moves that lead to empty cells
            movesToEmpty = filter (not . flip M.member space) <$> moves
            -- only moves that lead to empty cells and don't intersect the chain
            notIntersectingMoves = filter (not . intersectsChain space start) <$> movesToEmpty
            --space with inserted curent atom
            newSpace = M.insert start (Bead $ BeadInfo start ev glInd chainNo indOnChain) space
            --the i-th element of endings is how the state would look the rest of the chain be located in space
            --if we chose at this point i-th legalMove (not intersecting nor coliding)
            endings = notIntersectingMoves >>=
                mapM (\m -> catch $ addChain chainNo (indOnChain + 1) (glInd + 1) m newSpace evs)

-- |Tries to find a free field
getRandomFreePosition :: (MonadRandom m) => Int -> Space -> RepetitionGuardT m Vec3
getRandomFreePosition r space = do
    nextRep
    [x, y, z] <- replicateM 3 $ getRandomR (-r, r)
    let ans = V3 x y z
    if M.member ans space || quadrance ans > fromIntegral r
      then getRandomFreePosition r space else return ans

-- |Places a binder of random type in a space
placeBinder :: (MonadRandom m) =>
    Int
 -- ^Radius
 -> Int
 -- ^Number of binder types (excluding lamin),
 -- must be a non-negative integer
 -> Space
 -- ^Current space
 -> RepetitionGuardT m Space
 -- ^Space with added binder
placeBinder r types space = do
    pos <- getRandomFreePosition r space
    binderType <- getRandomR (1, types)
    return $ M.insert pos (Binder (BinderInfo pos (BinderType binderType))) space

-- |Finds a good starting point for a chain and places it there
hookAndPlaceChain :: (MonadRandom m) =>
    Int
 -- ^Radius
 -> Int
 -- ^ChainNumber
 -> Int
 -- ^Global index of firs bead in the chain
 -> [EnergyVector]
 -- ^Energy vectors of beads in the chain
 -> Space
 -- ^Current space
 -> RepetitionGuardT m Space
hookAndPlaceChain r nr glInd vectors space = do
    start <- getRandomFreePosition r space
    result <- catch $ addChain nr 0 glInd start space vectors
    maybe (hookAndPlaceChain r nr glInd vectors space) return result

-- |Places all the chains somewhere in the space
placeChains :: (MonadRandom m) => Int -> Space -> [[EnergyVector]] -> RepetitionGuardT m Space
placeChains r space vectors =
    fst <$> foldM (\(s,count) (ev, nr) -> (,count + length ev) <$>
        hookAndPlaceChain r nr count ev s) (space, 0) (zip vectors [0..])
