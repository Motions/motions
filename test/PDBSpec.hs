{-# LANGUAGE RecordWildCards #-}

module PDBSpec where

import qualified Bio.Motions.Representation.Dump as D
import Bio.Motions.PDB.Internal
import Bio.Motions.Types

import qualified Data.Vector.Unboxed as U
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
        context "when creating the header" $ do
            let PDBHeader seqNum atomCnt step = head pdbData
            it "yields a correct sequence number" $
                seqNum `shouldBe` headerSeqNum
            it "yields a correct atom count" $
                atomCnt `shouldBe` atomCount
            it "yields a correct header step" $
                step `shouldBe` headerStep

        it "yields the title second" $
            let isTitle PDBTitle {} = True
                isTitle _           = False
                titleSecond = isTitle . head. tail $ pdbData
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
                forM_ (D.binders dump) $ \binder ->
                    length (filter (isThisBinder binder) atomData) `shouldBe` 1
            it "yields every bead once" $
                forM_ (concat $ D.chains dump) $ \bead ->
                    length (filter (isThisBead bead) atomData) `shouldBe` 1

        let connectData = snd . span isAtomEntry $ atomData
        it "yields CONECT entries last" $
            let isConnect PDBConnect {} = True
                isConnect _             = False
            in all isConnect connectData `shouldBe` True
        it "yields all connections" $
            let getSerial bead = serial . fromJust . find (isThisBead bead) $ atomData
                serialChains = (getSerial <$>) <$> D.chains dump
                connectPairs = concat $ (zip <*> tail) <$> serialChains
            in forM_ connectPairs $ \(a, b) ->
                length (filter (isThisConnect a b) connectData) `shouldBe` 1
  where
    frameHeader = FrameHeader{..}
    meta = PDBMeta{..}
    headerSeqNum = 123
    headerStep = 456
    headerTitle = "headerTitle"
    beadRes ev | ev == ev0 = "AAA"
               | ev == ev1 = "AAB"
               | ev == ev2 = "AAC"
    binderRes bi | bi == bi0 = "BAA"
                 | bi == bi1 = "BAB"
                 | bi == bi2 = "BAC"
    chainId ch | ch == 0 = 'A'
               | ch == 1 = 'B'
    dump = D.Dump
        { D.radius = 10
        , D.binders =
            [ BinderInfo (V3 0 0 0) bi0
            , BinderInfo (V3 0 0 2) bi1
            , BinderInfo (V3 0 0 4) bi2
            ]
        , D.chains =
            [ [ BeadInfo (V3 1 1 0) be0 ev0 0 0 0
              , BeadInfo (V3 2 2 0) be1 ev1 1 0 1
              , BeadInfo (V3 3 3 0) be2 ev2 2 0 2
              ]
            , [ BeadInfo (V3 4 4 0) be1 ev1 3 1 0
              ]
            ]
        , D.beadKinds = [ev0, ev1, ev2]
        }
    [bi0, bi1, bi2] = BinderType <$> [1,2,3] -- no lamins
    [be0, be1, be2] = BeadType <$> [0,1,2]
    [ev0, ev1, ev2] = EnergyVector . U.fromList <$> [[1,0,0,0],[0,1,0,0],[0,0,1,0]]
    atomCount = (sum . map length . D.chains) dump + (length . D.binders) dump
    connectCount = sum . map  (flip (-) 1 . length) . D.chains $ dump

    isThisBinder (BinderInfo pos typ) PDBAtom{..} =
        name == "O" && resName == binderRes typ && chainID == ' ' && coords == toCoordData pos
    isThisBinder _ _ = False

    isThisBead (BeadInfo pos typ ev _ ch _) PDBAtom{..} =
        name == "C" && resName == beadRes ev && chainID == chainId ch && coords == toCoordData pos
    isThisBead _ _ = False

    isThisConnect a b PDBConnect{..} = a == fstSerial && b == sndSerial

spec :: Spec
spec = describe "the PDB converter" test
