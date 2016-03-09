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
import Data.Either
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
    beadRes = [ (ev0, "AAA")
              , (ev1, "AAB")
              , (ev2, "AAC")
              ]
    binderRes = [ (bi0, "BAA")
                , (bi1, "BAB")
                , (bi2, "BAC")
                ]
    chainId = [ (0, 'A')
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

testRead :: Spec
testRead = do
    context "when reading correct meta data" $ do
        let eitherErrMeta = toRevPDBMeta pdbMetaEntries
        it "reads the data without errors" $
            isRight eitherErrMeta `shouldBe` True
        when (isRight eitherErrMeta) $ do
            let Right meta = eitherErrMeta
            it "creates a correct bead resName map" $
                revBeadRes meta `shouldBe` revBeadRes expectedMeta
            it "creates a correct binder resName map" $
                revBinderRes meta `shouldBe` revBinderRes expectedMeta
            it "creates a correct chain id map" $
                revChainId meta `shouldBe` revChainId expectedMeta

    context "when reading correct frame data" $ do
        let eitherErrDump = fromPDBData expectedMeta pdbEntries
        it "reads the data without errors" $
            isRight eitherErrDump `shouldBe` True
        when (isRight eitherErrDump) $ do
            let Right dump = eitherErrDump
            it "creates all binders" $
                dumpBinders dump `shouldSatisfy` (`contains` dumpBinders expectedDump)
            it "creates only existing binders" $
                dumpBinders expectedDump `shouldSatisfy` (`contains` dumpBinders dump)
            it "creates all beads" $
                dumpBeads dump `shouldSatisfy` (`contains` dumpBeads expectedDump)
            it "creates only existing beads" $
                dumpBeads expectedDump `shouldSatisfy` (`contains` dumpBeads dump)
            it "creates all and only existing chains" $ do
                dumpChains dump `shouldSatisfy` (`unordContains` dumpChains expectedDump)
                dumpChains expectedDump `shouldSatisfy` (`unordContains` dumpChains dump)
            it "creates the chains in correct order" $
                and (zipWith unordEq (dumpChains dump) (dumpChains expectedDump)) `shouldBe` True
            it "creates the chains with correctly ordered beads" $
                forM_ (dumpChains dump) $ \c -> c `shouldSatisfy` (`elem` dumpChains expectedDump)
  where
    pdbEntries :: [PDBEntry]
    pdbEntries = [ PDBAtom 0 "C" ev0s ch0s 1 (V3 0 0 0)
                 , PDBAtom 1 "C" ev1s ch0s 2 (V3 0 0 3)
                 , PDBAtom 2 "O" bi0s ' ' 1 (V3 3 3 3)
                 , PDBConnect 0 1
                 ]
    pdbMetaEntries :: [PDBMetaEntry]
    pdbMetaEntries = [ EnergyVectorMap ev0 ev0s
                     , EnergyVectorMap ev1 ev1s
                     , BinderTypeMap bi0 bi0s
                     , BinderTypeMap bi1 bi1s
                     , ChainIdMap ch0 ch0s
                     ]

    expectedMeta = RevPDBMeta
        { revBeadRes = [ (ev0s, ev0)
                       , (ev1s, ev1)
                       ]
        , revBinderRes = [ (bi0s, bi0)
                         , (bi1s, bi1)
                         ]
        , revChainId = [ (ch0s, ch0)
                       ]
        }

    expectedDump :: Dump
    expectedDump = Dump
        { dumpBinders =
            [ BinderInfo (V3 1 1 1) bi0
            ]
        , dumpChains =
            [ [ DumpBeadInfo (V3 0 0 0) ev0
              , DumpBeadInfo (V3 0 0 1) ev1
              ]
            ]
        }

    (bi0, bi1) = (BinderType 0, BinderType 1)
    (bi0s, bi1s) = ("BAA", "BAB")

    (ev0, ev1) = ([0,0], [0,1])
    (ev0s, ev1s) = ("AAA", "AAB")

    ch0 = 0
    ch0s = 'a'

    dumpBeads :: Dump -> [DumpBeadInfo]
    dumpBeads = concat . dumpChains

    contains :: Eq a => [a] -> [a] -> Bool
    contains xs ys = null $ ys \\ xs

    unordEq :: Eq a => [a] -> [a] -> Bool
    unordEq xs ys = contains xs ys && contains ys xs

    unordContains :: Eq a => [[a]] -> [[a]] -> Bool
    unordContains xs ys = null $ deleteFirstsBy unordEq ys xs

spec :: Spec
spec = describe "the PDB converter" $ test >> testRead
