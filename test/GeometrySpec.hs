{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module GeometrySpec where

import Test.Hspec

import Bio.Motions.Types
import Bio.Motions.Common
import Bio.Motions.Representation.Common
import Bio.Motions.Representation.Dump
import Bio.Motions.Utils.Geometry

import qualified Data.HashMap.Strict as M
import Data.List
import Control.Monad
import Control.Lens
import Linear

chainsSpace :: [[Vec3]] -> Space
chainsSpace poss = M.fromList [(bead ^. position, asAtom bead) | bead <- beads]
  where beads = concat . dumpIndexedChains . Dump [] . (map . map $ \pos -> DumpBeadInfo pos []) $ poss

testSqrt2IntersectsChain :: Spec
testSqrt2IntersectsChain = do
    it "reports actual intersections to exist" $
        sqrt2IntersectsChain space (V3 7 8 7) (V3 7 7 8) `shouldBe` True

    it "doesn't report about non-existing intersections" $
        sqrt2IntersectsChain space (V3 7 8 7) (V3 7 8 8) `shouldBe` False
  where
    space = [ (V3 7 7 7, Located (V3 7 7 7) $ BeadSig $ BeadSignature ev 0 0 0)
            , (V3 7 8 8, Located (V3 7 8 8) $ BeadSig $ BeadSignature ev 0 0 1)
            ]
    ev = []

testIntersectsChain :: Spec
testIntersectsChain = do
    it "doesn't report about non-existing intersections" $
        assertFailure notIntersecting
    it "reports about existing intersections" $
        assertSuccess intersecting
  where
    assertFailure :: [[Vec3]] -> Expectation
    assertFailure chainsPoss =
        let space = chainsSpace chainsPoss
            connectedPoss = concatMap (zip <*> tail) chainsPoss
            maxQd = foldr (max . uncurry qd) 0 connectedPoss
         in forM_ connectedPoss $ \p -> uncurry (intersectsChain maxQd space) p `shouldBe` False

    assertSuccess :: ([[Vec3]], (Vec3, Vec3)) -> Expectation
    assertSuccess (chainsPoss, (p1, p2)) =
        let maxQd = foldr (max . uncurry qd) 0 . concatMap (zip <*> tail) $ chainsPoss
         in intersectsChain maxQd (chainsSpace chainsPoss) p1 p2 `shouldBe` True

    notIntersecting = [ [ V3 0 0 0, V3 2 0 0, V3 2 2 0, V3 1 4 0, V3 (-1) 2 0, V3 (-1) 1 0
                        , V3 1 1 1, V3 2 1 (-1), V3 1 1 (-1), V3 2 2 (-1), V3 0 1 0 ]
                      ]

    intersecting = ([ [ V3 0 0 0, V3 2 0 0 ]
                    ], (V3 1 1 0, V3 1 (-1) 0))

testChainIntersectsMove :: Spec
testChainIntersectsMove = do
    it "doesn't report when the move doesn't intersect with a chain" $
        assertFailure notIntersecting
    it "doesn't report when the move is between the moved bead's neighbours \
        \and doesn't intersect with a chain" $
        assertFailure notIntersectingDegenerated
    it "doesn't report when the moved bead has no neighbours and doesn't intersect with a chain" $
        assertFailure notIntersectingNoNeighbours
    it "doesn't report when the neighbours are on one side of the move and the move doesn't intersect \
        \with a chain" $
        assertFailure notIntersectingNeighboursOnOneSide
    it "reports when a segment connecting the moved bead with a neighbour cuts another neighbour" $
        assertSuccess neighbourToBeadCutsAnotherNeighbour
    it "reports when a segment connected to a neighbour of the moved bead cuts the segment connecting \
        \the neighbour with the moved bead but doesn't cut the moved bead" $
        assertSuccess neighbourOutCutsInNotMoved
    it "reports when a segment connected to a neighbour of the moved bead cuts the moved bead" $
        assertSuccess neighbourOutCutsMoved
    it "reports when a segment connected to a neighbour of the moved bead cuts a segment connecting \
        \another neighbour to the moved bead" $
        assertSuccess neighbourOutCutsAnotherNeighbourIn
    it "reports when a segment not connected to any neighbour cuts a segment connecting the moved bead \
        \with a neighbour" $
        assertSuccess notNeighbourIntersecting
    it "reports when the moved bead cuts its neighbour" $
        assertSuccess movedBeadCutsNeighbour
    it "reports when the moved bead has no neighbours and cuts a chain" $
        assertSuccess intersectingNoNeighbours
  where
    assertOutcome :: Bool -> ([[Vec3]], Move) -> Expectation
    assertOutcome outcome (chainsPoss, move@Move{..}) =
        let space = chainsSpace chainsPoss
            Just chain = find (elem moveFrom) chainsPoss
            Just idx = elemIndex moveFrom chain
            neighbours = [chain !! (idx - 1) | idx > 0] ++ [chain !! (idx + 1) | idx < length chain - 1]
            maxQd = foldr (max . uncurry qd) 0 . concatMap (zip <*> tail) $ chainsPoss
         in chainIntersectsMove maxQd space move neighbours `shouldBe` outcome

    assertFailure :: ([[Vec3]], Move) -> Expectation
    assertFailure = assertOutcome False

    assertSuccess :: ([[Vec3]], Move) -> Expectation
    assertSuccess = assertOutcome True

    notIntersecting = ([ [ V3 0 0 0, V3 0 2 0, V3 1 4 0, V3 (-1) 2 0, V3 (-1) 1 0
                         , V3 1 1 1, V3 2 1 0, V3 1 1 (-1), V3 2 2 (-1) ]
                       ], MoveFromTo (V3 0 2 0) (V3 2 2 0))

    notIntersectingDegenerated = ([ [ V3 0 0 0, V3 0 2 0, V3 0 4 0, V3 0 3 1
                                    , V3 (-1) 4 (-1), V3 1 2 0, V3 1 0 0 ]
                                  ], MoveFromTo (V3 0 2 0) (V3 0 3 0))

    notIntersectingNoNeighbours = ([ [ V3 0 2 0 ]
                                   , [ V3 0 4 0, V3 2 2 0, V3 0 0 0, V3 (-1) 3 0
                                     , V3 0 3 (-1), V3 1 2 0, V3 1 3 1 ]
                                   ], MoveFromTo (V3 0 2 0) (V3 0 3 0))

    notIntersectingNeighboursOnOneSide = ([ [ V3 (-1) 3 0, V3 0 0 0, V3 1 3 0 ]
                                          ], MoveFromTo (V3 0 0 0) (V3 42 0 0))

    neighbourToBeadCutsAnotherNeighbour = ([ [ V3 2 1 0, V3 0 0 0, V3 2 2 0 ]
                                           ], MoveFromTo (V3 0 0 0) (V3 3 0 0))

    neighbourOutCutsInNotMoved = ([ [ V3 0 0 0, V3 1 3 0, V3 1 2 0 ]
                                  ], MoveFromTo (V3 0 0 0) (V3 2 0 0))

    neighbourOutCutsMoved = ([ [ V3 0 0 0, V3 1 1 0, V3 1 (-1) 0 ]
                             ], MoveFromTo (V3 0 0 0) (V3 2 0 0))


    neighbourOutCutsAnotherNeighbourIn = ([ [ V3 (-3) 2 0, V3 0 0 0, V3 1 (-3) 0
                                            , V3 (-1) 1 0 ]
                                          ], MoveFromTo (V3 0 0 0) (V3 2 0 0))

    notNeighbourIntersecting = ([ [ V3 0 0 0, V3 0 3 0 ]
                                , [ V3 1 1 1, V3 1 1 (-1) ]
                                ], MoveFromTo (V3 0 0 0) (V3 3 0 0))

    movedBeadCutsNeighbour = ([ [ V3 0 0 0, V3 1 0 0 ]
                              ], MoveFromTo (V3 0 0 0) (V3 2 0 0))

    intersectingNoNeighbours = ([ [ V3 0 0 0 ]
                                , [ V3 1 (-1) 0, V3 1 1 0 ]
                                ], MoveFromTo (V3 0 0 0) (V3 2 0 0))

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
    it "reports a segment strictly inside the triangle" $
        assertSuccess strictlyInside
    it "doesn't report about a nonintersecting noncoplanar segment" $
        assertFailure notIntersectingNonCoplanar
    it "doesn't report about a nonintersecting coplanar segment" $
        assertFailure notIntersectingCoplanar
  where
      assertOutcome :: Bool -> (Triangle, Segment) -> Expectation
      assertOutcome outcome (Triangle{..}, Segment{..}) =
          zipWithM_ (\config which -> (uncurry intersectsTriangle config, which) `shouldBe` (outcome, which))
              [ (Triangle p1 p2 p3, Segment p q)
              , (Triangle p1 p3 p2, Segment p q)
              , (Triangle p1 p2 p3, Segment q p)
              , (Triangle p1 p3 p2, Segment q p)
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
      strictlyInside = (Triangle (V3 0 0 0) (V3 5 0 0) (V3 0 0 5), Segment (V3 1 0 1) (V3 1 0 2))

      t1 :: Triangle
      t1 = Triangle (V3 0 0 0) (V3 0 0 2) (V3 0 2 2)

      t2 :: Triangle
      t2 = Triangle (V3 0 0 0) (V3 0 0 3) (V3 0 3 3)

testIntersectsSegment :: Spec
testIntersectsSegment = do
    it "reports when coplanar noncolinear segments cross" $
        assertSuccess crossCoplanarNoncolinear
    it "reports when noncolinear segment ends at the other segment's interior" $
        assertSuccess endsAtInteriorNoncolinear
    it "reports when noncolinear segments have a common vertex" $
        assertSuccess commonVertexNoncolinear
    it "reports when colinear segments have a common vertex" $
        assertSuccess commonVertexColinear
    it "reports when one segment is contained in the other" $
        assertSuccess contained
    it "reports when two noncoplanar segments intersect" $
        assertSuccess crossNoncoplanar
    it "doesn't report when coplanar noncolinear segments don't intersect" $
        assertFailure notIntersectingCoplanarNoncolinear
    it "doesn't report when colinear segments don't intersect" $
        assertFailure notIntersectingColinear
    it "doesn't report when skew segments don't intersect" $
        assertFailure notIntersectingSkew
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

    crossCoplanarNoncolinear = (s, Segment (V3 2 1 0) (V3 1 3 0))
    endsAtInteriorNoncolinear = (s, Segment (V3 3 1 0) (V3 2 2 0))
    commonVertexNoncolinear = (s, Segment (V3 5 1 0) (V3 4 4 0))
    commonVertexColinear = (s, Segment (V3 4 4 0) (V3 5 5 0))
    contained = (s, Segment (V3 2 2 0) (V3 3 3 0))
    crossNoncoplanar = (Segment (V3 1 1 1) (V3 3 3 3), Segment (V3 0 3 2) (V3 4 1 2))
    notIntersectingCoplanarNoncolinear = (s, Segment (V3 1 2 0) (V3 0 3 0))
    notIntersectingColinear = (s, Segment (V3 5 5 0) (V3 6 6 0))
    notIntersectingSkew = (Segment (V3 0 0 0) (V3 3 3 0), Segment (V3 3 0 1) (V3 0 3 1))

    s :: Segment
    s = Segment (V3 1 1 0) (V3 4 4 0)

testPointInsideSegment :: Spec
testPointInsideSegment = do
    it "reports when the point is strictly inside the segment" $
        assertSuccess strictlyInside
    it "reports when the point is on the segment's vertex" $
        assertSuccess onVertex
    it "doesn't report when the point is not inside the segment" $
        assertFailure notInside
  where
    assertSuccess :: (Point, Segment) -> Expectation
    assertSuccess (p, s) = pointInsideSegment p s `shouldBe` True

    assertFailure :: (Point, Segment) -> Expectation
    assertFailure (p, s) = pointInsideSegment p s `shouldBe` False

    strictlyInside = (V3 1 1 1, seg)
    onVertex = (V3 2 2 2, seg)
    notInside = (V3 42 21 128, seg)

    seg :: Segment
    seg = Segment (V3 0 0 0) (V3 3 3 3)

testPointInsideTriangle :: Spec
testPointInsideTriangle = do
    it "reports when the point is strictly inside the triangle" $
        assertSuccess strictlyInside
    it "reports when the point is on an edge of the triangle" $
        assertSuccess onEdge
    it "reports when the point is one of the triangle's vertices" $
        assertSuccess onVertex
    it "doesn't report when the point is coplanar with but not in the triangle" $
        assertFailure coplanarNotInside
    it "doesn't report when the point is noncoplanar with the triangle" $
        assertFailure noncoplanar
  where
    assertOutcome :: Bool -> (Point, Triangle) -> Expectation
    assertOutcome outcome (p, Triangle{..}) =
        zipWithM_ (\config which -> (uncurry pointInsideTriangle config, which) `shouldBe` (outcome, which))
            [ (p, Triangle p1 p2 p3)
            , (p, Triangle p2 p1 p3)
            , (p, Triangle p3 p1 p2)
            ] ([1..] :: [Int])

    assertSuccess :: (Point, Triangle) -> Expectation
    assertSuccess = assertOutcome True

    assertFailure :: (Point, Triangle) -> Expectation
    assertFailure = assertOutcome False

    strictlyInside = (V3 2 2 0, t)
    onEdge = (V3 1 4 0, t)
    onVertex = (V3 0 5 0, t)
    coplanarNotInside = (V3 (-1) 0 0, t)
    noncoplanar = (V3 2 2 1, t)

    t :: Triangle
    t = Triangle (V3 0 0 0) (V3 0 5 0) (V3 5 0 0)

testVectorInsideAngle :: Spec
testVectorInsideAngle = do
    it "reports when the vector is strictly inside the angle" $
        assertSuccess strictlyInside
    it "reports when the vector is on on angle's edge" $
        assertSuccess onEdge
    it "reports when the vector is zero" $
        assertSuccess zeroVector
    it "reports when the vector is inside a zero angle" $
        assertSuccess zeroAngle
    it "reports when one of the angle's vectors is zero" $
        assertSuccess degeneratedZero
    it "reports when the angle's vectors have opposite directions" $
        assertSuccess degeneratedOpposite
    it "doesn't report then the vector is coplanar with, but not inside the angle" $
        assertFailure notInsideCoplanar
    it "doesn't report when the vector is not coplanar with the angle" $
        assertFailure notCoplanar
    it "doesn't report when the vector is not collinear with and not inside a zero angle" $
        assertFailure notInsideZero
    it "doesn't report when the vector is collinear with, but not inside a zero angle" $
        assertFailure notInsideCollinear
  where
    assertOutcome :: Bool -> (Vec3, Angle) -> Expectation
    assertOutcome outcome (v, Angle{..}) =
        zipWithM_ (\config which -> (uncurry vectorInsideAngle config, which) `shouldBe` (outcome, which))
            [ (v, Angle v1 v2)
            , (v, Angle v2 v1)
            ] ([1..] :: [Int])

    assertSuccess :: (Vec3, Angle) -> Expectation
    assertSuccess = assertOutcome True

    assertFailure :: (Vec3, Angle) -> Expectation
    assertFailure = assertOutcome False

    strictlyInside = (V3 1 42 0, a)
    onEdge = (V3 (-2) 10 0, a)
    zeroVector = (zero, a)
    zeroAngle = (V3 3 9 0, Angle (V3 1 3 0) (V3 2 6 0))
    degeneratedZero = (V3 42 0 0, Angle zero (V3 0 1 0))
    degeneratedOpposite = (V3 42 0 0, Angle (V3 0 (-1) 0) (V3 0 1 0))
    notInsideCoplanar = (V3 2 3 0, a)
    notCoplanar = (V3 0 2 1, a)
    notInsideZero = (V3 0 2 1, Angle (V3 0 2 0) (V3 0 3 0))
    notInsideCollinear = (V3 0 (-1) 0, Angle (V3 0 2 0) (V3 0 3 0))

    a :: Angle
    a = Angle (V3 (-1) 5 0) (V3 1 3 0)

testPointInsideRay :: Spec
testPointInsideRay = do
    it "reports when the ray goes through a point which is not the ray's origin" $
        assertSuccess insideNotOrigin
    it "reports when the ray goes through its origin" $
        assertSuccess origin
    it "doesn't report when the point is collinear with, but on the other side of the ray" $
        assertFailure collinearNotInside
    it "doesn't report when the point is noncollinear with the ray" $
        assertFailure nonCollinear
  where
    assertSuccess :: (Point, Ray) -> Expectation
    assertSuccess (p, r) = pointInsideRay p r `shouldBe` True

    assertFailure :: (Point, Ray) -> Expectation
    assertFailure (p, r) = pointInsideRay p r `shouldBe` False

    insideNotOrigin = (V3 3 2 0, ray)
    origin = (V3 1 0 0, ray)
    collinearNotInside = (V3 0 (-1) 0, ray)
    nonCollinear = (V3 3 2 1, ray)

    ray :: Ray
    ray = Ray (V3 1 0 0) (V3 1 1 0)

spec :: Spec
spec = do
    context "the sqrt2IntersectsChain function"
        testSqrt2IntersectsChain

    context "the intersectsChain function"
        testIntersectsChain

    context "the chainIntersectsMove function"
        testChainIntersectsMove

    context "the intersectsTriangle function"
        testIntersectsTriangle

    context "the intersectsSegment function"
        testIntersectsSegment

    context "the pointInsideTriangle function"
        testPointInsideTriangle

    context "the vectorInsideAngle function"
        testVectorInsideAngle

    context "the pointInsideRay function"
        testPointInsideRay
