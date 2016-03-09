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

testWrite :: Spec
testWrite = do
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
    context "when reading correct meta data" $
        it "creates the correct structure" $
            toRevPDBMeta pdbMetaEntries `shouldBe` Right expectedMeta
    context "when reading correct frame data" $ do
        let eitherErrDump = fromPDBData expectedMeta pdbEntries
        it "reads the data without errors" $
            eitherErrDump `shouldSatisfy` isRight
        when (isRight eitherErrDump) $ do
            let Right dump = eitherErrDump
            it "creates the correct binders" $
                dumpBinders dump `shouldMatchList` dumpBinders expectedDump
            it "creates the correct chains" $
                dumpChains dump `shouldBe` dumpChains expectedDump
    context "when reading incorrect meta data" $ do
        it "detects duplicate energy vector strings" $
            shouldFailParsingMeta metaDupEvStrs
        it "detects duplicate binder type strings" $
            shouldFailParsingMeta metaDupBtStrs
        it "detects duplicate chain id strings" $
            shouldFailParsingMeta metaDupChStrs
        it "detects duplicate energy vectors" $
            shouldFailParsingMeta metaDupEvs
        it "detects duplicate binder types" $
            shouldFailParsingMeta metaDupBts
        it "detects duplicate chain ids" $
            shouldFailParsingMeta metaDupChs
        it "detects when the mapping for lamin type is missing" $
            shouldFailParsingMeta metaMissingLamin
        it "detects when the binder types are not subsequent natural numbers" $
            shouldFailParsingMeta metaNotSubsequentBts
        it "detects when the length of energy vectors don't match the number of binder types" $
            shouldFailParsingMeta metaWrongEvLens
    context "when reading incorrect frame data" $ do
        it "fails when encountering an unknown binder residue name" $
            shouldFailParsingPDB pdbUnknownBinderRes
        it "fails when encountering an unknown bead residue name" $
            shouldFailParsingPDB pdbUnknownBeadRes
        it "fails when encountering an unknown chain id" $
            shouldFailParsingPDB pdbUnknownChainId
        it "detects two atoms not in a connect sequence but with the same chain id" $
            shouldFailParsingPDB pdbChainIdNotConnected
        it "detects two connections going out of one atom" $
            shouldFailParsingPDB pdbTwoConnectsOut
        it "detects two connections going into one atom" $
            shouldFailParsingPDB pdbTwoConnectsIn
        it "detects connection cycles" $
            shouldFailParsingPDB pdbConnectCycle
        it "detects connections going out to non-existing atoms" $
            shouldFailParsingPDB pdbConnectNonExisting
        it "detects connections between different chain ids" $
            shouldFailParsingPDB pdbConnectTwoChains
        it "detects connections between atoms that are placed too far from each other" $
            shouldFailParsingPDB pdbQdGreaterThan2
    context "when merging correct and compatible dumps" $ do
        let eitherErrMergedDump = mergeDumps [expectedDumpPart1, expectedDumpPart2, expectedDumpPart3]
        it "merges them without errors" $
            eitherErrMergedDump `shouldSatisfy` isRight
        when (isRight eitherErrMergedDump) $ do
            let Right mergedDump = eitherErrMergedDump
            it "creates the correct binders" $
                dumpBinders mergedDump `shouldMatchList` dumpBinders expectedDump
            it "creates the correct chains" $
                dumpChains mergedDump `shouldBe` dumpChains expectedDump
    context "when merging incorrect or incompatible dumps" $ do
        it "detects two atoms from a single dump occupying the same position" $
            mergeDumps [dumpTwoAtomsOnePos] `shouldSatisfy` isLeft
        it "detects crossings of chains from a single dump" $
            mergeDumps [dumpChainCross] `shouldSatisfy` isLeft
        it "detects two atoms from different dumps occupying the same position" $
            mergeDumps [dump1, dump2] `shouldSatisfy` isLeft
        it "detects crossings of chains from different dumps" $
            mergeDumps [dump1, dump3] `shouldSatisfy` isLeft
  where
    pdbEntries :: [PDBEntry]
    pdbEntries = [ PDBAtom 0 "C" ev0s ch0s 1 (V3 0 0 0)
                 , PDBAtom 1 "C" ev1s ch0s 2 (V3 0 0 3)
                 , PDBAtom 2 "C" ev0s ch1s 1 (V3 3 0 0)
                 , PDBAtom 3 "C" ev0s ch1s 2 (V3 6 0 0)
                 , PDBAtom 4 "C" ev0s ch1s 3 (V3 9 3 0)
                 , PDBAtom 5 "O" bi0s ' ' 1 (V3 0 3 0)
                 , PDBAtom 6 "O" bi1s ' ' 1 (V3 3 3 3)
                 , PDBConnect 0 1
                 , PDBConnect 2 3
                 , PDBConnect 3 4
                 ]
    pdbMetaEntries :: [PDBMetaEntry]
    pdbMetaEntries = [ EnergyVectorMap ev0 ev0s
                     , EnergyVectorMap ev1 ev1s
                     , BinderTypeMap bi0 bi0s
                     , BinderTypeMap bi1 bi1s
                     , ChainIdMap ch0 ch0s
                     , ChainIdMap ch1 ch1s
                     ]

    expectedMeta = RevPDBMeta
        { revBeadRes = [ (ev0s, ev0)
                       , (ev1s, ev1)
                       ]
        , revBinderRes = [ (bi0s, bi0)
                         , (bi1s, bi1)
                         ]
        , revChainId = [ (ch0s, ch0)
                       , (ch1s, ch1)
                       ]
        }

    expectedDump :: Dump
    expectedDump = Dump
        { dumpBinders =
            [ BinderInfo (V3 0 1 0) bi0
            , BinderInfo (V3 1 1 1) bi1
            ]
        , dumpChains =
            [ [ DumpBeadInfo (V3 0 0 0) ev0
              , DumpBeadInfo (V3 0 0 1) ev1
              ]
            , [ DumpBeadInfo (V3 1 0 0) ev0
              , DumpBeadInfo (V3 2 0 0) ev0
              , DumpBeadInfo (V3 3 1 0) ev0
              ]
            ]
        }

    expectedDumpPart1 :: Dump
    expectedDumpPart1 = Dump
        { dumpBinders =
            [ BinderInfo (V3 0 1 0) bi0
            , BinderInfo (V3 1 1 1) bi1
            ]
        , dumpChains = []
        }

    expectedDumpPart2 :: Dump
    expectedDumpPart2 = Dump
        { dumpBinders = []
        , dumpChains =
            [ [ DumpBeadInfo (V3 0 0 0) ev0
              , DumpBeadInfo (V3 0 0 1) ev1
              ]
            ]
        }

    expectedDumpPart3 :: Dump
    expectedDumpPart3 = Dump
        { dumpBinders = []
        , dumpChains =
            [ [ DumpBeadInfo (V3 1 0 0) ev0
              , DumpBeadInfo (V3 2 0 0) ev0
              , DumpBeadInfo (V3 3 1 0) ev0
              ]
            ]
        }

    (bi0, bi1, bi2) = (BinderType 0, BinderType 1, BinderType 2)
    (bi0s, bi1s, bi2s) = ("BAA", "BAB", "BAC")

    (ev0, ev1, evshort, evlong) = ([0,0], [0,1], [], [0,0,0])
    (ev0s, ev1s, evshorts, evlongs) = ("AAA", "AAB", "AAC", "AAD")

    (ch0, ch1) = (0, 1)
    (ch0s, ch1s) = ('a', 'b')

    shouldFailParsingMeta x = toRevPDBMeta x `shouldSatisfy` isLeft
    shouldFailParsingPDB x = fromPDBData expectedMeta x `shouldSatisfy` isLeft

    metaDupEvStrs :: [PDBMetaEntry]
    metaDupEvStrs = [ EnergyVectorMap ev0 ev0s
                    , EnergyVectorMap ev1 ev0s
                    , BinderTypeMap bi0 bi0s
                    , BinderTypeMap bi1 bi1s
                    , ChainIdMap ch0 ch0s
                    ]
    metaDupBtStrs :: [PDBMetaEntry]
    metaDupBtStrs = [ EnergyVectorMap ev0 ev0s
                    , BinderTypeMap bi0 bi0s
                    , BinderTypeMap bi1 bi0s
                    , ChainIdMap ch0 ch0s
                    ]
    metaDupChStrs :: [PDBMetaEntry]
    metaDupChStrs = [ EnergyVectorMap ev0 ev0s
                    , BinderTypeMap bi0 bi0s
                    , BinderTypeMap bi1 bi1s
                    , ChainIdMap ch0 ch0s
                    , ChainIdMap ch1 ch0s
                    ]
    metaDupEvs :: [PDBMetaEntry]
    metaDupEvs = [ EnergyVectorMap ev0 ev0s
                 , EnergyVectorMap ev0 ev1s
                 , BinderTypeMap bi0 bi0s
                 , BinderTypeMap bi1 bi1s
                 , ChainIdMap ch0 ch0s
                 ]
    metaDupBts :: [PDBMetaEntry]
    metaDupBts = [ EnergyVectorMap ev0 ev0s
                 , BinderTypeMap bi0 bi0s
                 , BinderTypeMap bi0 bi1s
                 , ChainIdMap ch0 ch0s
                 ]
    metaDupChs :: [PDBMetaEntry]
    metaDupChs = [ EnergyVectorMap ev0 ev0s
                 , BinderTypeMap bi0 bi0s
                 , BinderTypeMap bi1 bi1s
                 , ChainIdMap ch0 ch0s
                 , ChainIdMap ch0 ch1s
                 ]
    metaMissingLamin :: [PDBMetaEntry]
    metaMissingLamin = [ EnergyVectorMap evshort evshorts
                       , ChainIdMap ch0 ch1s
                       ]
    metaNotSubsequentBts :: [PDBMetaEntry]
    metaNotSubsequentBts = [ EnergyVectorMap evlong evlongs
                           , BinderTypeMap bi0 bi0s
                           , BinderTypeMap bi2 bi2s
                           , ChainIdMap ch0 ch1s
                           ]
    metaWrongEvLens :: [PDBMetaEntry]
    metaWrongEvLens = [ EnergyVectorMap evlong evlongs
                      , BinderTypeMap bi0 bi0s
                      , BinderTypeMap bi1 bi1s
                      , ChainIdMap ch0 ch1s
                      ]

    pdbUnknownBinderRes :: [PDBEntry]
    pdbUnknownBinderRes = [ PDBAtom 0 "O" "UNK" ' ' 1 (V3 0 0 0)
                          ]
    pdbUnknownBeadRes :: [PDBEntry]
    pdbUnknownBeadRes = [ PDBAtom 0 "C" "UNK" ch0s 1 (V3 0 0 0)
                        ]
    pdbUnknownChainId :: [PDBEntry]
    pdbUnknownChainId = [ PDBAtom 0 "C" ev0s 'u' 1 (V3 0 0 0)
                        ]
    pdbChainIdNotConnected :: [PDBEntry]
    pdbChainIdNotConnected = [ PDBAtom 0 "C" ev0s ch0s 1 (V3 0 0 0)
                             , PDBAtom 1 "C" ev0s ch0s 2 (V3 0 0 3)
                             ]
    pdbTwoConnectsOut :: [PDBEntry]
    pdbTwoConnectsOut = [ PDBAtom 0 "C" ev0s ch0s 1 (V3 0 0 0)
                        , PDBAtom 1 "C" ev0s ch0s 2 (V3 0 0 3)
                        , PDBAtom 2 "C" ev0s ch0s 3 (V3 0 0 9)
                        , PDBConnect 0 1
                        , PDBConnect 0 2
                        ]
    pdbTwoConnectsIn :: [PDBEntry]
    pdbTwoConnectsIn = [ PDBAtom 0 "C" ev0s ch0s 1 (V3 0 0 0)
                       , PDBAtom 1 "C" ev0s ch0s 2 (V3 0 0 3)
                       , PDBAtom 2 "C" ev0s ch0s 3 (V3 0 0 9)
                       , PDBConnect 0 1
                       , PDBConnect 2 1
                       ]
    pdbConnectCycle :: [PDBEntry]
    pdbConnectCycle = [ PDBAtom 0 "C" ev0s ch0s 1 (V3 0 0 0)
                      , PDBAtom 1 "C" ev0s ch0s 2 (V3 0 0 3)
                      , PDBAtom 2 "C" ev0s ch0s 3 (V3 0 0 9)
                      , PDBConnect 0 1
                      , PDBConnect 1 2
                      , PDBConnect 2 1
                      ]
    pdbConnectNonExisting :: [PDBEntry]
    pdbConnectNonExisting = [ PDBAtom 0 "C" ev0s ch0s 1 (V3 0 0 0)
                            , PDBConnect 0 1
                            ]
    pdbConnectTwoChains :: [PDBEntry]
    pdbConnectTwoChains = [ PDBAtom 0 "C" ev0s ch0s 1 (V3 0 0 0)
                          , PDBAtom 1 "C" ev0s ch1s 2 (V3 0 0 3)
                          , PDBConnect 0 1
                          ]
    pdbQdGreaterThan2 :: [PDBEntry]
    pdbQdGreaterThan2 = [ PDBAtom 0 "C" ev0s ch0s 1 (V3 0 0 0)
                        , PDBAtom 1 "C" ev0s ch0s 2 (V3 0 0 6)
                        , PDBConnect 0 1
                        ]

    dumpTwoAtomsOnePos :: Dump
    dumpTwoAtomsOnePos = Dump
        { dumpBinders =
            [ BinderInfo (V3 0 0 0) bi0
            , BinderInfo (V3 0 0 0) bi1
            ]
        , dumpChains = []
        }
    dumpChainCross :: Dump
    dumpChainCross = Dump
        { dumpBinders = []
        , dumpChains =
            [ [ DumpBeadInfo (V3 0 0 0) ev0
              , DumpBeadInfo (V3 1 1 0) ev0
              ]
            , [ DumpBeadInfo (V3 1 0 0) ev0
              , DumpBeadInfo (V3 0 1 0) ev0
              ]
            ]
        }
    dump1 :: Dump
    dump1 = Dump
        { dumpBinders =
            [ BinderInfo (V3 0 0 1) bi0
            ]
        , dumpChains =
            [ [ DumpBeadInfo (V3 0 0 0) ev0
              , DumpBeadInfo (V3 1 1 0) ev0
              ]
            ]
        }
    dump2 :: Dump
    dump2 = Dump
        { dumpBinders =
            [ BinderInfo (V3 0 0 1) bi0
            ]
        , dumpChains = []
        }
    dump3 :: Dump
    dump3 = Dump
        { dumpBinders = []
        , dumpChains =
            [ [ DumpBeadInfo (V3 0 1 0) ev0
              , DumpBeadInfo (V3 1 0 0) ev0
              ]
            ]
        }

spec :: Spec
spec = describe "the PDB converter" $ testWrite >> testRead
