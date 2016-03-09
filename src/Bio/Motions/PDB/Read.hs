{- |
Module      : Bio.Motions.PDB.Read
Description : Reading from the PDB format.
License:    : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Bio.Motions.PDB.Read ( readPDB
                            , readPDBMeta
                            ) where

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.PDB.Internal
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Parsec

import Control.Lens
import Control.Arrow
import Control.Monad.State as S
import Control.Monad.Except
import Text.ParserCombinators.Parsec as P
import qualified Bio.PDB.EventParser.PDBEvents as PE
import qualified Bio.PDB.EventParser.PDBEventParser as PP
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import System.IO
import Linear

readPDB :: [Handle] -> RevPDBMeta -> IO (Either ReadError Dump)
readPDB hs meta = (toDump =<<) . sequence <$> mapM readPDBData hs
  where
    toDump :: [[PDBEntry]] -> Either ReadError Dump
    toDump = (mergeDumps =<<) . mapM (fromPDBData meta)

readPDBMeta :: Handle -> IO (Either ReadError RevPDBMeta)
readPDBMeta h = (toRevPDBMeta =<<) <$> readPDBMetaData h

mergeDumps :: [Dump] -> Either ReadError Dump
mergeDumps dumps = checkChains >> checkPositions >> pure (Dump allBinders allChains)
  where
    allBinders = dumps >>= dumpBinders
    allChains = dumps >>= dumpChains
    allAtomPositions = map dumpBeadPosition (concat  allChains)
                    ++ map (^. position) allBinders
    checkChains = pure () -- TODO: chain ids
    checkPositions = foldM_ checkStep Set.empty allAtomPositions
    checkStep s p = unless (Set.member p s) (throwError ("Two atoms occupying the same position: " ++ show p))
                 >> pure (Set.insert p s)

-- |Reads the first frame in a PDB file --TODO: last?
readPDBData :: Handle -> IO (Either ReadError [PDBEntry])
readPDBData h = parseFrame . fst . BS.breakSubstring "END" <$> BS.hGetContents h
  where
    parseFrame :: BS.ByteString -> Either ReadError [PDBEntry]
    parseFrame frame = do
        let headerLine : rest = BS.lines frame
        header <- parseHeader $ BS.unpack headerLine
        (title, rest') <- flip catchError (const $ pure ([], rest)) $ do
            let titleLine : rest' = rest
            title <- parseTitle $ BS.unpack titleLine
            pure ([title], rest')
        execStateT (parseRecords $ BS.unlines rest') $ title ++ [header]

    parseRecords :: BS.ByteString -> S.StateT [PDBEntry] (Either ReadError) ()
    parseRecords str = PP.parsePDBRecords "" str act ()

    act :: t -> PE.PDBEvent -> (S.StateT [PDBEntry] (Either ReadError)) ()
    act _ event = lift (fromEvent event) >>= \e -> modify (e :)

parseHeader :: String -> Either ReadError PDBEntry
parseHeader = parseOrError header
  where header = PDBHeader <$> (string "HEADER" *> spaces *> many anyChar)

parseTitle :: String -> Either ReadError PDBEntry
parseTitle = parseOrError title
  where title = PDBTitle <$> (string "TITLE" *> spaces *> many (P.noneOf [' ']) <* spaces)

parseOrError :: Parser a -> String -> Either ReadError a
parseOrError p = left show . runParser p () ""

fromEvent :: PE.PDBEvent -> Either ReadError PDBEntry
fromEvent PE.ATOM{..} = Right PDBAtom
    { serial = no
    , name = BS.unpack atomtype
    , resName = BS.unpack restype
    , chainID = chain
    , resSeq = resid
    , coords = case coords of PE.Vector3 x y z -> V3 x y z
    }
fromEvent (PE.CONECT [fstSerial, sndSerial]) = Right PDBConnect{..}
fromEvent a = Left $ "Error/unknown entry: " ++ show a
