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
module Bio.Motions.PDB.Read (readPDBData
                            ,fromPDBData
                            ,parseFrame
                            ) where

import Bio.Motions.PDB.Internal

import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Parsec
import qualified Bio.PDB.EventParser.PDBEvents as PE
import qualified Bio.PDB.EventParser.PDBEventParser as PP
import qualified Data.ByteString.Char8 as BS
import System.IO
import Linear

-- |Reads lines for the next frame in a PDB file.
readPDBData :: Handle -> IO [BS.ByteString]
readPDBData h = rdloop []
  where
    rdloop acc = BS.hGetLine h >>= \case
                        "END" -> pure $ reverse acc
                        "" -> rdloop acc  -- ignore empty line
                        x -> rdloop (x:acc)

parseFrame :: [BS.ByteString] -> Either ReadError [PDBEntry]
parseFrame [] = Left "unexpected end of file"
parseFrame (headerLine:rest) = do
    header <- parseHeader headerLine
    (title, rest') <- flip catchError (const $ pure ([], rest)) $ do
        let titleLine : rest' = rest
        title <- parseTitle titleLine
        pure ([title], rest')
    fmap reverse . execStateT (parseRecords $ BS.unlines rest') $ title ++ [header]
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
    , coords = case coords of PE.Vector3 x y z -> round <$> V3 x y z
    }
fromEvent (PE.CONECT [fstSerial, sndSerial]) = Right PDBConnect{..}
fromEvent a = Left $ "Error/unknown entry: " ++ show a
