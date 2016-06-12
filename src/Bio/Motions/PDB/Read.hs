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
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.IO
import Linear

-- |Reads a dump from PDB files, taking the next frame from each file and merging them.
readPDB ::
     [Handle]
  -- ^A list of handles to the PDB files.
  -> RevPDBMeta
  -- ^The Meta structure used to read the PDB format.
  -> Maybe Int
  -- ^Square of the maximum chain segment length or Nothing if unbounded. Used for error checking.
  -> IO (Either ReadError Dump)
  -- ^The resulting dump or error.
readPDB hs meta maxd = (sequence >=> toDump) <$> mapM readPDBData hs
  where
    toDump :: [[PDBEntry]] -> Either ReadError Dump
    toDump = mapM (fromPDBData meta maxd) >=> mergeDumps

-- |Reads the next frame in a PDB file.
readPDBData :: Handle -> IO (Either ReadError [PDBEntry])
readPDBData h = parseFrame <$> rdloop BSL.empty
  where
    rdloop acc = BS.hGetLine h >>= \case
                        "END" -> return acc -- TODO eof
                        x -> rdloop . BSL.append acc . BSL.fromStrict $ x

parseFrame :: BSL.ByteString -> Either ReadError [PDBEntry]
parseFrame frame = do
    let headerLine : rest = BSL.lines frame
    header <- parseHeader headerLine
    (title, rest') <- flip catchError (const $ pure ([], rest)) $ do
        let titleLine : rest' = rest
        title <- parseTitle titleLine
        pure ([title], rest')
    execStateT (parseRecords $ BSL.unlines rest') $ title ++ [header]
  where
    parseRecords :: BSL.ByteString -> StateT [PDBEntry] (Either ReadError) ()
    parseRecords str = PP.parsePDBRecords "" (BSL.toStrict str) onEvent ()

    onEvent :: t -> PE.PDBEvent -> StateT [PDBEntry] (Either ReadError) ()
    onEvent _ event = lift (fromEvent event) >>= \e -> modify (e :)

parseHeader :: BSL.ByteString -> Either ReadError PDBEntry
parseHeader = parseOrError header
  where header = PDBHeader <$> (string "HEADER" *> spaces *> many anyChar)

parseTitle :: BSL.ByteString -> Either ReadError PDBEntry
parseTitle = parseOrError title
  where title = PDBTitle <$> (string "TITLE" *> spaces *> many (noneOf [' ']) <* spaces <* eof)

fromEvent :: PE.PDBEvent -> Either ReadError PDBEntry
fromEvent PE.ATOM{..} = Right PDBAtom
    { serial = no
    , name = BS.unpack atomtype
    , resName = BS.unpack restype
    , chainID = chain
    , resSeq = resid
    , coords = case coords of PE.Vector3 x y z -> round <$> V3 x y z
    }
fromEvent (PE.CONECT [fstSerial, sndSerial]) = Right PDBConnect{..}
fromEvent a = Left $ "Error/unknown entry: " ++ show a
