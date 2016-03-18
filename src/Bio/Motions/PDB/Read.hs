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
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Bio.Motions.PDB.Read ( readPDB
                            ) where

import Bio.Motions.PDB.Internal
import Bio.Motions.Representation.Dump

import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Parsec
import qualified Bio.PDB.EventParser.PDBEvents as PE
import qualified Bio.PDB.EventParser.PDBEventParser as PP
import qualified Data.ByteString.Char8 as BS
import System.IO
import Linear

readPDB :: [Handle] -> RevPDBMeta -> IO (Either ReadError Dump)
readPDB hs meta = (sequence >=> toDump) <$> mapM readPDBData hs
  where
    toDump :: [[PDBEntry]] -> Either ReadError Dump
    toDump = mapM (fromPDBData meta) >=> mergeDumps

-- |Reads the first frame in a PDB file
readPDBData :: Handle -> IO (Either ReadError [PDBEntry])
readPDBData h = parseFrame . fst . BS.breakSubstring "END" <$> BS.hGetContents h

parseFrame :: BS.ByteString -> Either ReadError [PDBEntry]
parseFrame frame = do
    let headerLine : rest = BS.lines frame
    header <- parseHeader headerLine
    (title, rest') <- flip catchError (const $ pure ([], rest)) $ do
        let titleLine : rest' = rest
        title <- parseTitle titleLine
        pure ([title], rest')
    execStateT (parseRecords $ BS.unlines rest') $ title ++ [header]
  where
    parseRecords :: BS.ByteString -> StateT [PDBEntry] (Either ReadError) ()
    parseRecords str = PP.parsePDBRecords "" str onEvent ()

    onEvent :: t -> PE.PDBEvent -> StateT [PDBEntry] (Either ReadError) ()
    onEvent _ event = lift (fromEvent event) >>= \e -> modify (e :)

parseHeader :: BS.ByteString -> Either ReadError PDBEntry
parseHeader = parseOrError header
  where header = PDBHeader <$> (string "HEADER" *> spaces *> many anyChar)

parseTitle :: BS.ByteString -> Either ReadError PDBEntry
parseTitle = parseOrError title
  where title = PDBTitle <$> (string "TITLE" *> spaces *> many (noneOf [' ']) <* spaces <* eof)

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
