{- |
Module      : Bio.Motions.PDB.Meta
Description : Structures that allow conversion from/to the PDB format.
License:    : MIT
Stability   : experimental
Portability : unportable
 -}
module Bio.Motions.PDB.Meta ( PDBMeta
                            , mkPDBMeta
                            , mkSimplePDBMeta
                            , legacyPDBMeta
                            , writePDBMeta
                            ) where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.PDB.Internal

import qualified Data.Map as M
import Control.Monad
import System.IO

writePDBMeta :: Handle -> [EnergyVector] -> [BinderType] -> [Int] -> PDBMeta -> IO ()
writePDBMeta h evs bts chs = writePDBMetaData h . toPDBMetaData evs bts chs

-- |Creates a 'PDBMeta' structure containing data needed when converting to the PDB format.
mkPDBMeta ::
    [EnergyVector] -- ^ Energy vectors used through the simulation.
 -> [BinderType] -- ^ Binder types used through the simulation.
 -> [ChainId] -- ^ Chain identifiers used through the simulation.
 -> Maybe PDBMeta -- ^ The resulting structure if the given sets weren't too large.
mkPDBMeta evs bts chs = PDBMeta <$> mapEnergyVectors evs <*> mapBinderTypes bts <*> mapChains chs

-- |Creates a 'PDBMeta' structure that describes a PDB file in which all binders, except lamins,
-- have one type. The @resName@ field of an @ATOM@ PDB record in such a file is compatible with
-- the prototype simulation's output.
mkSimplePDBMeta ::
    (ChainId -> Char) -- ^ A mapping from chain identifiers to characters defined in 'chars'.
 -> PDBMeta
mkSimplePDBMeta = PDBMeta simpleBeadRes simpleBinderRes
  where
    simpleBeadRes ev | doesNotBind ev = "UNB"
                     | bindsWithLamins ev  = "LAM"
                     | otherwise = "BOU"
    simpleBinderRes bt | isLamin bt   = "LAM"
                       | otherwise = "BIN"

-- |A 'PDBMeta' structure that describes a PDB file compatible with the prototype simulation's output.
-- Assumes that there is only one chain in the simulation.
legacyPDBMeta :: PDBMeta
legacyPDBMeta = mkSimplePDBMeta $ const ' '

writePDBMetaData :: Handle -> [PDBMetaEntry] -> IO ()
writePDBMetaData handle = mapM_ $ hPutStrLn handle . show

-- |Creates an injective mapping from a set of 'EnergyVector's to 'String's of length 3 made of characters
-- defined in 'chars' and not starting with a @B@. The set must be at most as large as the counterdomain.
mapEnergyVectors :: [EnergyVector] -> Maybe (EnergyVector -> String)
mapEnergyVectors evs = guard (length evs <= length vals) >> pure ((M.!) mapping)
  where vals = [[a, b, c] | [a, b, c] <- replicateM 3 chars, a /= 'B']
        mapping = M.fromList $ zip evs vals

-- |Creates an injective mapping from a set of 'BinderType's to 'String's of length 3 made of characters
-- defined in 'chars' and starting with a @B@. The set must be at most as large as the counterdomain.
mapBinderTypes :: [BinderType] -> Maybe (BinderType -> String)
mapBinderTypes bts = guard (length bts <= length vals) >> pure ((M.!) mapping)
  where vals = [['B', a, b] | [a, b] <- replicateM 2 chars]
        mapping = M.fromList $ zip bts vals

-- |Creates an injective mapping from a set of 'ChainId's to characters defined in 'chars'.
-- The set must be at most as large as the counterdomain.
mapChains :: [ChainId] -> Maybe (ChainId -> Char)
mapChains chs = guard (length chs <= length chars) >> pure ((M.!) mapping)
  where mapping = M.fromList $ zip chs chars

-- |Characters used in string/character fields of PDB entries.
chars :: [Char]
chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
