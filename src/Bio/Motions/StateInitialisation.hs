{- |
Module      : Bio.Motions.StateInitialisation
Description : Random initialisation of state for new simulation.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Bio.Motions.StateInitialisation where

import Control.Lens
import Control.Monad
import Control.Monad.Random
import qualified Data.Map.Strict as M
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
initialise :: (MonadRandom m) =>
     Int
  -- ^Maximum number of initialisation attempts
  -> Int
  -- ^Radius
  -> [Int]
  -- ^Number of binders of each type excluding lamin,
  -- the order must be the same as the order used in energy vectors
  -> [[EnergyVector]]
  -- ^EnergyVectors of beads
  -> m (Maybe Dump)
  -- ^Initial state
initialise maxTries r bindersCounts evs = runRepetitionGuardT maxTries $
    spaceToDump <$> initialiseSpace r bindersCounts evs

-- |Creates an initial state
initialiseSpace :: (MonadRandom m) =>
     Int
  -- ^Radius
  -> [Int]
  -- ^Number of binders of each type excluding lamin,
  -- the order must be the same as the order used in energy vectors
  -> [[EnergyVector]]
  -- ^EnergyVectors of beads
  -> RepetitionGuardT m Space
  -- ^Initial state
initialiseSpace r bindersCounts beads = do
    stateWithChains <- addChains r stateWithLamins beads
    addBinders stateWithChains
  where
    stateWithLamins = addLamins M.empty $ spherePoints (fromIntegral r)
    addBinders = foldr (<=<) return . concat $
        zipWith (\count -> replicate count . addBinder r) bindersCounts [1..]

-- |Converts Space to Dump
spaceToDump :: Space -> Dump
spaceToDump space = Dump{..}
  where
    dumpBinders = [Located l b | Located l (BinderSig b) <- M.elems space]
    beads = [Located l b | Located l (BeadSig b) <- M.elems space]
    dumpChains = map (map dropIndices) $
        groupWith (view beadChain) $ sortWith (view beadAtomIndex) beads

-- |Generates all points such that their distance from (0,0,0)
-- is in the range [radius - 2, radius + 2]
spherePoints :: Int -> [Vec3]
spherePoints radius = [v | [x,y,z] <- replicateM 3 [-radius..radius],
                      let v = V3 x y z,
                      let q = quadrance v,
                      q >= (radius - 2)^2 && q <= (radius + 2)^2]

-- |Adds lamin binders into space
addLamins :: Space -> [Vec3] -> Space
addLamins space positions =
    space `M.union` M.fromList [(v3, asAtom $ BinderInfo v3 laminType) | v3 <- positions]

-- |Tries to add beads of one chain in the space
addChain :: (MonadRandom m) =>
    Int
 -- ^Index of the chain to be added
 -> Int
 -- ^Chain index of the first bead to be added
 -> Int
 -- ^Global index of the first atom to be added
 -> Vec3
 -- ^Starting point
 -> Space
 -- ^Current space
 -> [EnergyVector]
 -- ^Energy vectors of atoms that are going to be added
 -> RepetitionGuardT m Space
 -- ^The state with a added chain
addChain _ _ _ _ space [] = return space
addChain chainNo indOnChain glInd start space [ev] =
    return $ M.insert start (asAtom $ BeadInfo start ev glInd chainNo indOnChain) space
addChain chainNo indOnChain glInd start space (ev:evs) = do
    nextRep
    moves <- shuffleM $ V.toList legalMoves
    let targets = [target | move <- moves,
                            let target = move + start,
                            not $ target `M.member` space,
                            not $ intersectsChain space start target]
    let newSpace = M.insert start (asAtom $ BeadInfo start ev glInd chainNo indOnChain) space
    msum . map (\m -> addChain chainNo (indOnChain + 1) (glInd + 1) m newSpace evs) $ targets

-- |Tries to find a free position
getRandomFreePosition :: (MonadRandom m) =>
    Int
 -- ^Radius
 -> Space
 -- ^Current space
 -> RepetitionGuardT m Vec3
getRandomFreePosition r space = go
  where
    go = do
        [x, y, z] <- replicateM 3 $ getRandomR (-r, r)
        let ans = V3 x y z
        if M.member ans space || quadrance ans > r^2
        then nextRep >> go
        else return ans

-- |Adds a binder of given type in a space
addBinder :: (MonadRandom m) =>
    Int
 -- ^Radius
 -> Int
 -- ^The binder's type
 -> Space
 -- ^Current space
 -> RepetitionGuardT m Space
 -- ^Space with added binder
addBinder r binderType space = do
    pos <- getRandomFreePosition r space
    return $ M.insert pos (asAtom $ BinderInfo pos $ BinderType binderType) space

-- |Finds a good starting point for a chain and adds it there
hookAndAddChain :: (MonadRandom m) =>
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
hookAndAddChain r chainNo glInd vectors space = go
  where
    go = do
        start <- getRandomFreePosition r space
        addChain chainNo 0 glInd start space vectors `mplus` (nextRep >> go)

-- |Adds all the chains somewhere in the space
addChains :: (MonadRandom m) =>
    Int
 -- ^Radius
  -> Space
 -- ^Current space
  -> [[EnergyVector]]
 -- ^Energy vectors of the beads in the chains
  -> RepetitionGuardT m Space
addChains r space vectors =
    fst <$> foldM (\(s, count) (evs, nr) -> (,count + length evs) <$>
        hookAndAddChain r nr count evs s) (space, 0) (zip vectors [0..])
