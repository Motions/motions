{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module FreezePredicateParserSpec where

import Bio.Motions.Types
import Bio.Motions.Representation.Common
import Bio.Motions.Utils.FreezePredicateParser
import GHC.Exts
import Data.Either
import Text.Parsec
import Test.Hspec
import Test.Hspec.SmallCheck

run :: String -> Either ParseError FreezePredicate
run = parse freezePredicateParser "<test>"

makeBeadSignature :: Int -> Int -> BeadSignature
makeBeadSignature chain indexOnChain = BeadSignature
    { _beadEV = fromList []
    , _beadAtomIndex = -1
    , _beadChain = chain
    , _beadIndexOnChain = indexOnChain
    }

prop :: String -> String -> (Int -> Int -> Bool) -> Spec
prop description (run -> Right parsed) predicate =
    it description . property $ \(chain, index) ->
        parsed (makeBeadSignature chain index) == predicate chain index

{-# ANN correctSpec "HLint: ignore Use ||" #-}
correctSpec :: Spec
correctSpec = do
    context "when parsing an empty file" $
        prop "freezes nothing" "" $ \_ _ ->
            False

    context "when parsing a single chain" $
        prop "freezes only this chain" "4" $ \chain _ ->
            chain == 4

    context "when parsing a chain range" $
        prop "freezes only those chains" "4-8" $ \chain _ ->
            chain `elem` [4..8]

    context "when parsing a bead range on a single chain" $
        prop "freezes only those beads" "1:2-10" $ \chain index ->
            chain == 1 && index `elem` [2..10]

    context "when parsing a bead range on multiple chains" $
        prop "freezes only those beads" "1-2:3-8" $ \chain index ->
            chain `elem` [1..2] && index `elem` [3..8]

    context "when parsing a wildcard range" $
        prop "freezes only those beads" "*:2-9" $ \_ index ->
            index `elem` [2..9]

    context "when parsing a negation of a chain" $
        prop "freezes everything except for this chain" "!3" $ \chain _ ->
            chain /= 3

    context "when parsing an alternative of chains" $
        prop "freezes only those chains" "1,3,9" $ \chain _ ->
            chain `elem` [1, 3, 9]

    context "when parsing a nested expression" $
        prop "freezes only those chains" "!(1,7)" $ \chain _ ->
            chain `notElem` [1, 7]

    context "when parsing a complex alternative" $
        prop "freezes only those chains" "1,2,3-5:4,9:6-8" $ \chain index ->
            or [ chain `elem` [1, 2]
               , chain `elem` [3..5] && index == 4
               , chain == 9 && index `elem` [6..8]
               ]

incorrectSpec :: Spec
incorrectSpec = do
    context "when parsing a non-integer" $
        it "fails" $ isLeft (run "helloworld") `shouldBe` True

    context "when parsing an unterminated range" $
        it "fails" $ isLeft (run "1-") `shouldBe` True

spec :: Spec
spec = describe "the FreezePredicate parser" $ do
    context "when parsing correct ranges"
        correctSpec

    context "when parsing incorrect ranges"
        incorrectSpec
