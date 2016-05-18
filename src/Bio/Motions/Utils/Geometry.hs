{- |
Module      : Bio.Motions.Utils.Geometry
Description : Utility geometry functions.
License     : Apache
Stability   : experimental
Portability : unportable
-}
{-# LANGUAGE RecordWildCards #-}
module Bio.Motions.Utils.Geometry where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Linear

type Point = V3 Int

data Triangle = Triangle
    { p1 :: Point
    , p2 :: Point
    , p3 :: Point
    }

data Segment = Segment
    { p :: Point
    , q :: Point
    }

data Angle = Angle
    { v1 :: V3 Int
    , v2 :: V3 Int
    }

data Ray = Ray
    { o :: Point
    , dir :: V3 Int
    }

data Cube = Cube
    { minCorner :: Point
    , maxCorner :: Point
    }

-- |Tests whether a segment intersects a triangle in 3D.
-- Gives correct results when the triangle is actually a segment.
-- Works well with all nondegenerate edge cases, e.g. intersection at a vertex.
-- Gives unspecified results in more degenerate cases, i.e. the triangle or the segment is a point.
-- Warning: possible integer overflows with large distances.
intersectsTriangle :: Triangle -> Segment -> Bool
intersectsTriangle tri@Triangle{..} seg
    | w /= 0 = isJust $ do
        let sgn = signum w
        guard $ sgn * s <= 0
        let w2 = a `cross` d
            t = w2 `dot` c
        guard $ sgn * t >= 0
        let u = - w2 `dot` b
        guard $ sgn * u >= 0
        let v = w - s - t - u
        guard $ sgn * v >= 0
    | s /= 0 = isJust $ do
        let sgn = signum s
        let w2 = d `cross` a
            t = w2 `dot` c
        guard $ sgn * t >= 0
        let u = - w2 `dot` b
        guard $ sgn * u >= 0
        let v = s - w - t - u
        guard $ sgn * v >= 0
    | otherwise = any (intersectsSegment seg) [Segment p1 p2, Segment p1 p3, Segment p2 p3]
               || pointInsideTriangle (p seg) tri
  where
    a = p seg - p3
    b = p1 - p3
    c = p2 - p3
    d = q seg - p3
    w1 = b `cross` c
    w = a `dot` w1
    s = d `dot` w1

-- |Tests whether two segments in 3D intersect.
-- Returns true for all nondegenerate edge cases, e.g. intersection at a vertex.
-- Gives unspecified results in degenerate cases.
-- Warning: possible integer overflows with large distances.
intersectsSegment :: Segment -> Segment -> Bool
intersectsSegment s1 s2 =
    let r = q s1 - p s1
        s = q s2 - p s2
        rxs = r `cross` s
        p1p2 = p s2 - p s1
    in if rxs == 0 && p1p2 `cross` r == 0 then
           let t0' = p1p2 `dot` r
               t1' = t0' + s `dot` r
               (t0, t1) = if s `dot` r < 0 then (t1', t0') else (t0', t1')
               dr = r `dot` r
            in (0 <= t0 && t0 <= dr) || (0 <= t1 && t1 <= dr) || (t0 <= 0 && dr <= t1)
       else
           let d1 = (p1p2 `cross` s) `dot` rxs
               d2 = (p1p2 `cross` r) `dot` rxs
               drxs = rxs `dot` rxs
            in rxs /= 0 && 0 <= d1 && d1 <= drxs && 0 <= d2 && d2 <= drxs
                        && drxs *^ p1p2 == d1 *^ r - d2 *^ s

-- |Tests whether a segment goes through a point.
-- Assumes that the segment is nondegenerate, i.e. it is not a point.
-- Warning: possible integer overflows with large distances.
pointInsideSegment :: Point -> Segment -> Bool
pointInsideSegment v Segment{..} =
    pv `cross` pq == 0 && 0 <= dpvpq && dpvpq <= dpqpq
  where
    pv = v - p
    pq = q - p
    dpvpq = pv `dot` pq
    dpqpq = pq `dot` pq

-- |Tests whether a point is (not necessarily strictly) inside a triangle.
-- Assumes that the three points defining the triangle are pairwise different.
-- Works well when the triangle is actually a segment.
-- Warning: possible integer overflows with large distances.
pointInsideTriangle :: Point -> Triangle -> Bool
pointInsideTriangle p t@Triangle{..} =
    det33 (V3 (p2 - p1) (p3 - p1) (p - p1)) == 0 && pointInsideTriangle2D p t

-- |Tests whether a point is (not necessarily strictly) inside a triangle.
-- Assumes that the three points defining the triangle are pairwise different.
-- Works well when the triangle is actually a segment.
-- Assumes that the point and the triangle are coplanar.
-- Warning: possible integer overflows with large distances.
pointInsideTriangle2D :: Point -> Triangle -> Bool
pointInsideTriangle2D p Triangle{..} =
    vectorInsideAngle p1p (Angle p1p2 p1p3) &&
    vectorInsideAngle p2p (Angle p2p1 p2p3) &&
    vectorInsideAngle p3p (Angle p3p1 p3p2)
  where
    p1p = p - p1
    p2p = p - p2
    p3p = p - p3
    p1p2 = p2 - p1
    p1p3 = p3 - p1
    p2p3 = p3 - p2
    p2p1 = - p1p2
    p3p1 = - p1p3
    p3p2 = - p2p3

-- |Tests whether a vector is (not necessarily strictly) inside an angle.
-- When any of the angle vectors is zero or the angle vectors have opposing directions,
-- returns true for all vectors.
-- Warning: possible integer overflows with large distances.
vectorInsideAngle :: V3 Int -> Angle -> Bool
vectorInsideAngle v a@Angle{..} = det33 (V3 v v1 v2) == 0 && vectorInsideAngle2D v a

-- |Tests whether a vector is (not necessarily strictly) inside an angle.
-- When any of the angle vectors is zero or the angle vectors have opposing directions,
-- returns true for all vectors.
-- Assumes that the vector and the angle are coplanar.
-- Warning: possible integer overflows with large distances.
vectorInsideAngle2D :: V3 Int -> Angle -> Bool
vectorInsideAngle2D v Angle{..} =
    v1cv2 * v1cv >= 0 && v2cv1 * v2cv >= 0 && v1cv * v2cv <= 0 &&
        (v1cv2 /= 0 || v `dot` v1 >= 0 || v `dot` v2 >= 0)
  where
    v1cv2 = v1 `cross` v2
    v1cv = v1 `cross` v
    v2cv = v2 `cross` v
    v2cv1 = - v1cv2

-- |Tests whether the given ray goes through the given point.
-- Assumes that the ray's vector is nonzero.
-- Warning: possible integer overflows with large distances.
pointInsideRay :: Point -> Ray -> Bool
pointInsideRay p Ray{..} =
    op `cross` dir == 0 && 0 <= op `dot` dir
  where op = p - o

-- |The bounding cube for a set of points.
-- Warning: possible integer overflows with large distances.
boundingCube :: [Point] -> Cube
boundingCube ps = Cube{..}
  where
    minCorner = foldr1 (liftA2 min) ps
    maxCorner = foldr1 (liftA2 max) ps

-- |A cube extended in each direction by the given radius.
-- Warning: possible integer overflows with large distances.
extendedCube :: Int -> Cube -> Cube
extendedCube radius Cube{..} = Cube minCorner' maxCorner'
  where
    minCorner' = fmap (\x -> x - radius) minCorner
    maxCorner' = fmap (+ radius) maxCorner
