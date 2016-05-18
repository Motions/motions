{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module GeometrySpec where

import Test.Hspec

import Bio.Motions.Types
import Bio.Motions.Representation.Common
import Bio.Motions.Utils.Geometry

import Control.Monad
import Linear

testIntersectsChain :: Spec
testIntersectsChain = do
    it "reports actual intersections to exist" $
        intersectsChain space (V3 7 8 7) (V3 7 7 8) `shouldBe` True

    it "doesn't report about non-existing intersections" $
        intersectsChain space (V3 7 8 7) (V3 7 8 8) `shouldBe` False
  where
    space = [ (V3 7 7 7, Located (V3 7 7 7) $ BeadSig $ BeadSignature ev 0 0 0)
            , (V3 7 8 8, Located (V3 7 8 8) $ BeadSig $ BeadSignature ev 0 0 1)
            ]
    ev = []

testIntersectsTriangle :: Spec
testIntersectsTriangle = do
    it "reports a noncoplanar segment going through the interior" $
        assertSuccess goingThroughInteriorNoncoplanar
    it "reports a noncoplanar segment ending in the interior" $
        assertSuccess endingAtInteriorNoncoplanar
    it "reports a noncoplanar segment going through an edge" $
        assertSuccess goingThroughEdgeNoncoplanar
    it "reports a noncoplanar segment ending at an edge" $
        assertSuccess endingAtEdgeNoncoplanar
    it "reports a noncoplanar segment going through a vertex" $
        assertSuccess goingThroughVertexNoncoplanar
    it "reports a noncoplanar segment ending at a vertex" $
        assertSuccess endingAtVertexNoncoplanar
    it "reports a coplanar segment going through the interior" $
        assertSuccess goingThroughInteriorCoplanar
    it "reports a coplanar segment going through an edge" $
        assertSuccess goingThroughEdgeCoplanar
    it "reports a coplanar segment ending at an edge" $
        assertSuccess endingAtEdgeCoplanar
    it "reports a coplanar segment going through a vertex" $
        assertSuccess goingThroughVertexCoplanar
    it "reports a coplanar segment ending at a vertex" $
        assertSuccess endingAtVertexCoplanar
    it "doesn't report about a nonintersecting noncoplanar segment" $
        assertFailure notIntersectingNonCoplanar
    it "doesn't report about a nonintersecting coplanar segment" $
        assertFailure notIntersectingCoplanar
  where
      assertOutcome :: Bool -> (Triangle, Segment) -> Expectation
      assertOutcome outcome (Triangle{..}, Segment{..}) =
          zipWithM_ (\config which -> (uncurry intersectsTriangle config, which) `shouldBe` (outcome, which))
              [ (Triangle v1 v2 v3, Segment p q)
              , (Triangle v1 v3 v2, Segment p q)
              , (Triangle v1 v2 v3, Segment q p)
              , (Triangle v1 v3 v2, Segment q p)
              ] ([1..] :: [Int])

      assertSuccess :: (Triangle, Segment) -> Expectation
      assertSuccess = assertOutcome True

      assertFailure :: (Triangle, Segment) -> Expectation
      assertFailure = assertOutcome False

      goingThroughInteriorNoncoplanar = (t1, Segment (V3 (-1) 0 2) (V3 1 1 1))
      endingAtInteriorNoncoplanar = (t2, Segment (V3 (-1) 0 2) (V3 0 1 2))
      goingThroughEdgeNoncoplanar = (t2, Segment (V3 (-1) 0 2) (V3 1 0 1))
      endingAtEdgeNoncoplanar = (t1, Segment (V3 (-1) 0 2) (V3 0 1 2))
      goingThroughVertexNoncoplanar = (t1, Segment (V3 (-1) 0 2) (V3 1 0 2))
      endingAtVertexNoncoplanar = (t1, Segment (V3 (-1) 0 2) (V3 0 2 2))
      goingThroughInteriorCoplanar = (t2, Segment (V3 0 1 0) (V3 0 1 2))
      goingThroughEdgeCoplanar = (t1, Segment (V3 0 1 1) (V3 0 3 3))
      endingAtEdgeCoplanar = (t1, Segment (V3 0 1 0) (V3 0 1 1))
      goingThroughVertexCoplanar = (t1, Segment (V3 0 (-1) 0) (V3 0 1 0))
      endingAtVertexCoplanar = (t1, Segment (V3 0 0 0) (V3 0 1 0))
      notIntersectingNonCoplanar = (t2, Segment (V3 (-2) 0 2) (V3 (-1) 1 2))
      notIntersectingCoplanar = (t2, Segment (V3 0 0 4) (V3 0 0 5))

      t1 :: Triangle
      t1 = Triangle (V3 0 0 0) (V3 0 0 2) (V3 0 2 2)

      t2 :: Triangle
      t2 = Triangle (V3 0 0 0) (V3 0 0 3) (V3 0 3 3)

testIntersectsSegment :: Spec
testIntersectsSegment = do
    it "reports when noncolinear segments cross" $
        assertSuccess crossNoncolinear
    it "reports when noncolinear segment ends at the other segment's interior" $
        assertSuccess endsAtInteriorNoncolinear
    it "reports when noncolinear segments have a common vertex" $
        assertSuccess commonVertexNoncolinear
    it "reports when colinear segments have a common vertex" $
        assertSuccess commonVertexColinear
    it "reports when one segment is contained in the other" $
        assertSuccess contained
    it "doesn't report when noncolinear segments don't intersect" $
        assertFailure notIntersectingNoncolinear
    it "doesn't report when colinear segments don't intersect" $
        assertFailure notIntersectingColinear
  where
    assertOutcome :: Bool -> (Segment, Segment) -> Expectation
    assertOutcome outcome (s1, s2) =
        zipWithM_ (\config which -> (uncurry intersectsSegment config, which) `shouldBe` (outcome, which))
            [ (s1, s2)
            , (s1, Segment (q s2) (p s2))
            , (s2, s1)
            , (s2, Segment (q s1) (p s1))
            ] ([1..] :: [Int])

    assertSuccess :: (Segment, Segment) -> Expectation
    assertSuccess = assertOutcome True

    assertFailure :: (Segment, Segment) -> Expectation
    assertFailure = assertOutcome False

    crossNoncolinear = (s, Segment (V3 2 1 0) (V3 1 3 0))
    endsAtInteriorNoncolinear = (s, Segment (V3 3 1 0) (V3 2 2 0))
    commonVertexNoncolinear = (s, Segment (V3 5 1 0) (V3 4 4 0))
    commonVertexColinear = (s, Segment (V3 4 4 0) (V3 5 5 0))
    contained = (s, Segment (V3 2 2 0) (V3 3 3 0))
    notIntersectingNoncolinear = (s, Segment (V3 1 2 0) (V3 0 3 0))
    notIntersectingColinear = (s, Segment (V3 5 5 0) (V3 6 6 0))

    s :: Segment
    s = Segment (V3 1 1 0) (V3 4 4 0)


spec :: Spec
spec = do
    context "the intersectsChain function"
        testIntersectsChain

    context "the intersectsTriangle function"
        testIntersectsTriangle

    context "the intersectsSegment function"
        testIntersectsSegment
