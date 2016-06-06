{- |
Module      : Bio.Motions.PDB.Meta
Description : Structures that allow conversion from/to the PDB format.
License:    : Apache
Stability   : experimental
Portability : unportable
 -}
module Bio.Motions.PDB.Meta ( PDBMeta
                            , mkPDBMeta
                            , mkSimplePDBMeta
                            , writePDBMeta
                            , readPDBMeta
                            , getChainNames
                            , mapChains
                            ) where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.PDB.Internal

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Data.List as L
import Control.Monad
import System.IO

writePDBMeta :: Handle -> PDBMeta -> IO ()
writePDBMeta h = writePDBMetaData h . toPDBMetaData

readPDBMeta :: Handle -> IO (Either ReadError RevPDBMeta)
readPDBMeta h = (toRevPDBMeta =<<) <$> readPDBMetaData h

-- |Extracts ChainNames form RevPDBMeta
getChainNames :: RevPDBMeta -> [String]
getChainNames = map fst . sortOn snd . M.toList . revChainName

-- |Creates a 'PDBMeta' structure containing data needed to convert to the PDB format.
mkPDBMeta ::
    [EnergyVector] -- ^The set of energy vectors used through the simulation.
 -> [BinderType] -- ^The set of binder types used through the simulation.
 -> [(ChainId, String)] -- ^The set of chain identifiers and their names used through the simulation.
 -> Maybe PDBMeta -- ^The resulting structure if the given sets weren't too large.
mkPDBMeta evs bts chs = PDBMeta <$> mapEnergyVectors evs <*> mapBinderTypes bts
                                <*> mapChains (fst <$> chs) <*> pure (M.fromList chs)

-- |Creates a 'PDBMeta' structure that describes a PDB file in which all binders, except lamins,
-- have one type. The @resName@ field of an @ATOM@ PDB record in such a file is compatible with
-- the prototype simulation's output.
mkSimplePDBMeta ::
    [EnergyVector] -- ^The set of energy vectors used through the simulation.
 -> [BinderType] -- ^The set of binder types used through the simulation.
 -> [(ChainId, String)] -- ^The set of chain identifiers and their names used through the simulation.
 -> Maybe PDBMeta
mkSimplePDBMeta evs bts chs = PDBMeta simpleBeadResMap simpleBinderResMap
                                <$> mapChains (fst <$> chs) <*> pure (M.fromList chs)
  where
    simpleBeadResMap = M.fromList . (zip <*> map simpleBeadRes) $ evs
    simpleBinderResMap = M.fromList . (zip <*> map simpleBinderRes) $ bts

    simpleBeadRes ev | doesNotBind ev = "UNB"
                     | bindsWithLamins ev = "LAM"
                     | otherwise = "BOU"
    simpleBinderRes bt | bt == laminType = "LAM"
                       | otherwise = "BIN"

writePDBMetaData :: Handle -> [PDBMetaEntry] -> IO ()
writePDBMetaData h = mapM_ $ hPutStrLn h . printPDBMetaEntry

readPDBMetaData :: Handle -> IO (Either ReadError [PDBMetaEntry])
readPDBMetaData h = parsePDBMetaData <$> BS.hGetContents h

-- |Creates an injective mapping from a set of 'EnergyVector's to 'String's of length 3 made of characters
-- defined in 'pdbChars' and not starting with a @B@. The set must be at most as large as the counterdomain.
mapEnergyVectors :: [EnergyVector] -> Maybe (M.Map EnergyVector String)
mapEnergyVectors evs = guard (length evs <= length vals) >> pure mapping
  where vals = [[a, b, c] | [a, b, c] <- replicateM 3 pdbChars, a /= 'B']
        mapping = M.fromList $ zip evs vals

-- |Creates an injective mapping from a set of 'BinderType's to 'String's of length 3 made of characters
-- defined in 'pdbChars' and starting with a @B@. The set must be at most as large as the counterdomain.
mapBinderTypes :: [BinderType] -> Maybe (M.Map BinderType String)
mapBinderTypes bts = guard (length bts <= length vals) >> pure mapping
  where vals = [['B', a, b] | [a, b] <- replicateM 2 pdbChars]
        mapping = M.fromList $ zip bts vals

-- |Creates an injective mapping from a set of 'ChainId's to characters defined in 'pdbChars'.
-- The set must be at most as large as the counterdomain.
mapChains :: [ChainId] -> Maybe (M.Map ChainId Char)
mapChains chs = guard (length chs <= length pdbChars) >> pure mapping
  where mapping = M.fromList $ zip chs pdbChars
