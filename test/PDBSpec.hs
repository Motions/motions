{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module PDBSpec where

import Bio.Motions.Representation.Dump
import Bio.Motions.PDB.Internal
import Bio.Motions.Types

import qualified Data.Vector.Unboxed as U
import qualified Data.Map as M
import Control.Monad
import Data.List
import Data.Maybe
import Linear
import Test.Hspec

test :: Spec
test = do
    let pdbData = toPDBData frameHeader meta dump
    context "when creating frame data" $ do
        it "yields the corrent number of entries" $
            length pdbData `shouldBe` 2 + atomCount + connectCount
        it "yields the header first" $
            let isHeader PDBHeader {} = True
                isHeader _            = False
                headerFirst = isHeader . head $ pdbData
            in headerFirst `shouldBe` True
        it "yields the title second" $
            let isTitle PDBTitle {} = True
                isTitle _           = False
                titleSecond = isTitle . head . tail $ pdbData
            in titleSecond `shouldBe` True
        it "yields a correct title" $
            let PDBTitle title = head . tail $ pdbData
            in title `shouldBe` "headerTitle"

        let atomData = tail . tail $ pdbData
            isAtomEntry PDBAtom {} = True
            isAtomEntry _          = False
            atomsEnd =  findIndex (not . isAtomEntry) atomData
        it "doesn't yield only ATOM entries after the title" $
            atomsEnd `shouldNotBe` Nothing
        it "yields a correct number of ATOM entries after the title" $
            atomsEnd `shouldBe` Just atomCount
        context "when creating the ATOM entries" $ do
            it "yields every binder once" $
                forM_ (dumpBinders dump) $ \binder ->
                    length (filter (isThisBinder binder) atomData) `shouldBe` 1
            it "yields every bead once" $
                forM_ (concat chains) $ \bead ->
                    length (filter (isThisBead bead) atomData) `shouldBe` 1

        let connectData = dropWhile isAtomEntry atomData
        it "yields CONECT entries last" $
            let isConnect PDBConnect {} = True
                isConnect _             = False
            in all isConnect connectData `shouldBe` True
        it "yields all connections" $
            let getSerial bead = serial . fromJust . find (isThisBead bead) $ atomData
                serialChains = (getSerial <$>) <$> chains
                connectPairs = concat $ (zip <*> tail) <$> serialChains
            in forM_ connectPairs $ \(a, b) ->
                length (filter (isThisConnect a b) connectData) `shouldBe` 1
  where
    frameHeader = StepHeader{..}
    meta = PDBMeta{..}
    headerSeqNum = 123
    headerStep = 456
    headerTitle = "headerTitle"
    beadRes = M.fromList [ (ev0, "AAA")
                         , (ev1, "AAB")
                         , (ev2, "AAC")
                         ]
    binderRes = M.fromList [ (bi0, "BAA")
                           , (bi1, "BAB")
                           , (bi2, "BAC")
                           ]
    chainId = M.fromList [ (0, 'A')
                         , (1, 'B')
                         ]
    dump = Dump
        { dumpBinders =
            [ BinderInfo (V3 0 0 0) bi0
            , BinderInfo (V3 0 0 2) bi1
            , BinderInfo (V3 0 0 4) bi2
            ]
        , dumpChains =
            [ [ DumpBeadInfo (V3 1 1 0) ev0
              , DumpBeadInfo (V3 2 2 0) ev1
              , DumpBeadInfo (V3 3 3 0) ev2
              ]
            , [ DumpBeadInfo (V3 4 4 0) ev1
              ]
            ]
        }
    [bi0, bi1, bi2] = map BinderType [1,2,3] -- no lamins
    (ev0, ev1, ev2) = ([1,0,0,0], [0,1,0,0], [0,0,1,0])
    chains = dumpIndexedChains dump
    atomCount = (sum . map length . dumpChains) dump + (length . dumpBinders) dump
    connectCount = sum . map  (flip (-) 1 . length) . dumpChains $ dump

    isThisBinder (BinderInfo pos typ) PDBAtom{..} =
        name == "O" && resName == binderRes M.! typ && chainID == ' ' && coords == toCoordData pos
    isThisBinder _ _ = False

    isThisBead (BeadInfo pos ev _ ch _) PDBAtom{..} =
        name == "C" && resName == beadRes M.! ev && chainID == chainId M.! ch && coords == toCoordData pos
    isThisBead _ _ = False

    isThisConnect a b PDBConnect{..} = a == fstSerial && b == sndSerial

spec :: Spec
spec = describe "the PDB converter" test
