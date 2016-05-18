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
    { v1 :: Point
    , v2 :: Point
    , v3 :: Point
    }

data Segment = Segment
    { p :: Point
    , q :: Point
    }

data Cube = Cube
    { minCorner :: Point
    , maxCorner :: Point
    }

-- TODO: haddock, reference
intersectsTriangle :: Triangle -> Segment -> Bool
intersectsTriangle Triangle{..} seg
    | w /= 0 = isJust $ do
        guard $ w * s <= 0
        let w2 = a `cross` d
            t = w2 `dot` c
        guard $ w * t >= 0
        let u = - w2 `dot` b
        guard $ w * u >= 0
        let v = w - s - t - u
        guard $ w * v >= 0
    | s /= 0 = isJust $ do
        let w2 = d `cross` a
            t = w2 `dot` c
        guard $ s * t >= 0
        let u = - w2 `dot` b
        guard $ s * u >= 0
        let v = s - w - t - u
        guard $ s * v >= 0
    | otherwise = any (intersectsSegment seg) [Segment v1 v2, Segment v1 v3, Segment v2 v3]
  where
    a = p seg - v3
    b = v1 - v3
    c = v2 - v3
    d = q seg - v3
    w1 = b `cross` c
    w = a `dot` w1
    s = d `dot` w1

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

boundingCube :: [Point] -> Cube
boundingCube ps = Cube{..}
  where
    minCorner = foldr1 (liftA2 min) ps
    maxCorner = foldr1 (liftA2 max) ps

extendedCube :: Int -> Cube -> Cube
extendedCube radius Cube{..} = Cube minCorner' maxCorner'
  where
    minCorner' = fmap (\x -> x - radius) minCorner
    maxCorner' = fmap (+ radius) maxCorner
