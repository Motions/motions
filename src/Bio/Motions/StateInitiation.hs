{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.StateInitiation where

import Bio.Motions.Representation.Dump
import Bio.Motions.Types
import Bio.Motions.Representation.Chain.Internal(Space, intersectsChain, legalMoves)
import Bio.Motions.RandomShuffle

import qualified Data.Map as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Control.Monad.Random
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import GHC.Exts(groupWith, sortWith)
import Data.List
import Data.Maybe
import Linear

-- |Creates an initial state
initialize :: (MonadRandom m) =>
     Int
  -- ^Maximal number of tries to initialize
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

-- |Creates an initial state in a form of space
initializeSpace :: (MonadRandom m) =>
     Int
  -- ^Radius
  -> Int
  -- ^Number of binders
  -> Int
  -- ^Number of binders' types
  -> [[EnergyVector]]
  -- ^EnergyVectors of beads
  -> MaybeT (RepetitionGuard m) Space
  -- ^Initial state
initializeSpace r bindersCount bindersTypesCount beads  = do
    stateWithChains <- placeChains r stateWithLamins beads
    foldM (\s _ -> placeBinder r bindersTypesCount s) stateWithChains [1..bindersCount]
  where
    stateWithLamins::Space
    stateWithLamins = addLamins M.empty $ spherePoints (fromIntegral r)

-- |Convers space to Dump
spaceToDump :: Int -> Space -> Dump
spaceToDump dumpRadius space = Dump{..}
    where
        isBinder :: Atom -> Bool
        isBinder (Binder _) = True
        isBinder _ = False
        isBead :: Atom -> Bool
        isBead (Bead _) = True
        isBead _ = False
        dumpBinders :: [BinderInfo]
        dumpBinders = map (\(Binder x) -> x) $ filter isBinder $ M.elems space
        beads :: [BeadInfo]
        beads = map (\(Bead x) -> x) $ filter isBead $ M.elems space
        dumpChains :: [[DumpBeadInfo]]
        dumpChains = map (map dropIndices) $ groupWith beadChain $ sortWith beadAtomIndex beads
        beadKinds = []

-- |Generates all points, such that their distance from (0,0,0)
-- is in the range [radius - 2, radius + 2]
spherePoints :: Double -> [Vec3]
spherePoints radius = do
    let r = (ceiling radius::Int) + 2
    x <- [-r .. r]
    let y_max = ceiling $ sqrt $ sq (radius + 2) - fromIntegral (sq x)
    y <- [-y_max .. y_max]
    let z_square_min = sq (radius - 2) - fromIntegral (sq x + sq y)
    let z_square_max = sq (radius + 2) - fromIntegral (sq x + sq y)
    let lower_bound = if z_square_min < 0 then 0 else ceiling $ sqrt z_square_min
    let upper_bound = if z_square_max < 0 then -1 else floor $ sqrt z_square_max
    abs_z <- [lower_bound .. upper_bound]
    z <- nub [abs_z, -abs_z]
    return $ V3 x y z
    where sq x = x * x

-- |Adds lamin binders into space
addLamins :: Space -> [Vec3] -> Space
addLamins = foldl (\dict v3 -> M.insert v3 (Binder BinderInfo{binderPosition=v3, binderType=BinderType 0}) dict)

-- |Monad used to conrol number of steps
type RepetitionGuard = StateT Int

-- |Tries to place beads of one chain in the space
addChain :: (MonadRandom m) =>
    Int
 -- ^Number of the chain we are placing
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
 ->  MaybeT (RepetitionGuard m) Space
 -- ^The state with a placed chain, or nothing if no initalization was found
addChain _ _ _ _ space [] = return space
addChain chainNo indOnChain glInd start space [ev] =
    return $ M.insert start (Bead $ BeadInfo start ev glInd chainNo indOnChain) space
addChain chainNo indOnChain glInd start space (ev:tl) = MaybeT $ do
    repsLeft <- get
    put $ repsLeft - 1
    if repsLeft <= 0 then return Nothing else listToMaybe . catMaybes <$> endings
        where
            -- all legal moves in random order
            moves::(MonadRandom m) => RepetitionGuard m [Vec3]
            moves = map (+start) <$> randomShuffleM (V.toList legalMoves)
            -- only moves that lead to empty cells
            movesToEmpty::(MonadRandom m) => RepetitionGuard m [Vec3]
            movesToEmpty = filter (not . flip M.member space) <$> moves
            -- only moves that lead to empty cells and don't intersect the chain
            notIntersectingMoves::(MonadRandom m) => RepetitionGuard m [Vec3]
            notIntersectingMoves = filter (not . intersectsChain space start) <$> movesToEmpty
            --space with inserted curent atom
            newSpace::Space
            newSpace = M.insert start (Bead $ BeadInfo start ev glInd chainNo indOnChain) space
            --the i-th element of endings is how the state would look the rest of the chain be located in space
            --if we chose at this point i-th legalMove (not intersecting nor coliding)
            endings :: (MonadRandom m) => RepetitionGuard m [Maybe Space]
            endings = notIntersectingMoves >>=
                mapM (\m -> runMaybeT $ addChain chainNo (indOnChain + 1) (glInd + 1) m newSpace tl)

-- |Tries to find a free field
getRandomFreeField::(MonadRandom m) => Int -> Space -> MaybeT (RepetitionGuard m) Vec3
getRandomFreeField r space = do
    repsLeft <- get
    put $ repsLeft - 1
    if repsLeft <= 0 then MaybeT $ return Nothing else do
        x <- lift $ lift $ getRandomR (-r, r)
        y <- lift $ lift $ getRandomR (-r, r)
        z <- lift $ lift $ getRandomR (-r, r)
        let ans = V3 x y z
        if M.member ans space || quadrance ans > fromIntegral r then getRandomFreeField r space else return ans

-- |Places a binder of random type in a space
placeBinder::(MonadRandom m) =>
    Int ->
 -- ^Radius
    Int ->
 -- ^Number of binder types (excluding lamin),
 -- must be a non-negative integer
    Space ->
 -- ^Current space
    MaybeT (RepetitionGuard m) Space
 -- ^Space with added binder
placeBinder r types space = do
    pos <- getRandomFreeField r space
    binderType <- lift $ lift $ getRandomR (1, types)
    return $ M.insert pos (Binder (BinderInfo pos (BinderType binderType))) space

-- |Finds a good starting point for a chain and places it there
hookAndPlaceChain::(MonadRandom m) =>
    Int ->
 -- ^Radius
    Int ->
 -- ^ChainNumber
    Int ->
 -- ^Global index of firs bead in the chain
    [EnergyVector] ->
 -- ^Energy vectors of beads in the chain
    Space ->
 -- ^Current space
    MaybeT (RepetitionGuard m) Space
hookAndPlaceChain r nr glInd vectors space = do
    start <- getRandomFreeField r space
    --this will also check if we haved exceded number of repetitions
    result <- lift $ runMaybeT $ addChain nr 0 glInd start space vectors
    maybe (hookAndPlaceChain r nr glInd vectors space) return result

-- |Places all the chains somewhere in the space
placeChains :: (MonadRandom m) => Int -> Space -> [[EnergyVector]] -> MaybeT (RepetitionGuard m) Space
placeChains r space vectors =
    fst <$> foldM (\(s,count) (ev, nr) -> (,count + length ev) <$>
        hookAndPlaceChain r nr count ev s) (space, 0) (zip vectors [0..])
