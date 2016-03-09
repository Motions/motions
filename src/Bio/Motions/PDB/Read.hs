{- |
Module      : Bio.Motions.PDB.Read
Description : Reading from the PDB format.
License:    : Apache
Stability   : experimental
Portability : unportable
 -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Bio.Motions.PDB.Read ( readPDB
                            ) where

import Bio.Motions.Types
import Bio.Motions.PDB.Internal
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Parsec

import Control.Arrow
import Control.Monad.State as S
import Text.ParserCombinators.Parsec as P
import qualified Bio.PDB.EventParser.PDBEvents as PE
import qualified Bio.PDB.EventParser.PDBEventParser as PP
import qualified Data.ByteString.Char8 as BS
import System.IO
import Linear

readPDB :: Handle -> RevPDBMeta -> IO (Either ReadError Dump)
readPDB h meta = do
    pdbData <- readPDBData h
    pure $ pdbData >>= fromPDBData meta

-- |Reads first frame in a PDB file --TODO: last? lamins?
readPDBData :: Handle -> IO (Either ReadError [PDBEntry])
readPDBData h = do
    header <- parseHeader <$> hGetLine h
    title <- parseTitle <$> hGetLine h
    rest <- fst . BS.breakSubstring "END" <$> BS.hGetContents h
    let parseRest = PP.parsePDBRecords "" rest act ()
        entries = do
            hd <- header
            tt <- title
            execStateT parseRest [tt, hd]
    print entries
    pure entries
  where
    act :: t -> PE.PDBEvent -> (S.StateT [PDBEntry] (Either ReadError)) ()
    act _ event = lift (fromEvent event) >>= \e -> modify (e :)

parseHeader :: String -> Either ReadError PDBEntry
parseHeader = left show . runParser header () ""
  where
    header :: Parser PDBEntry
    header = PDBHeader <$> (string "HEADER" *> spaces *> int <* spaces)
                       <*> (int <* spaces)
                       <*> (string "step" *> spaces *> int <* spaces)

parseTitle :: String -> Either ReadError PDBEntry
parseTitle = left show . runParser title () ""
  where
    title :: Parser PDBEntry
    title = PDBTitle <$> (string "TITLE" *> spaces *> many (P.noneOf [' ']) <* spaces)

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
