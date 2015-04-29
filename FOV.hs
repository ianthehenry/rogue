module FOV
( fieldOfView
, ShadowMap
, ObstructionMap
, Visibility(..)
, Opacity(..)
) where

import BasePrelude hiding (map, lookup)
import Types
import Data.Array
import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.Reader (ReaderT, ask, runReaderT)

type ScanInfo = (ObstructionMap, Int, (Delta, Delta))
type Scanning = ReaderT ScanInfo (Writer (Dual [Coord]))
type Slope = Rational

type ShadowMap = Array Coord Visibility
type ObstructionMap = Array Coord Opacity

data Visibility = Visible | Hidden deriving (Eq, Show)
data Opacity = Transparent | Opaque deriving (Eq)

lookup :: Ix i => i -> Array i e -> Maybe e
lookup i a | inRange (bounds a) i = Just (a ! i)
           | otherwise = Nothing

data Axis = X | Y
otherAxis X = Y
otherAxis Y = X

along :: Axis -> Int -> Delta
along X i = (i, 0)
along Y i = (0, i)

octants :: [(Delta, Delta)]
octants = do
  axis <- [X, Y]
  majorSign <- [1, -1]
  minorSign <- [1, -1]
  return (along axis majorSign, along (otherAxis axis) minorSign)

-- The bounds of obstructionMap must be a square centered at 0,0 with an odd
-- length side, or everything will crash. So, like, ((-10, -10), (10, 10)) is
-- a valid bounds choice.
fieldOfView :: ObstructionMap -> ShadowMap
fieldOfView obstructionMap = initialShadowMap // fmap (, Visible) visibleCoords
  where
    visibleCoords = getDual (execWriter runOctants)
    mapBounds = bounds obstructionMap
    (_, (squadius, _)) = mapBounds
    runOctants = forM_ octants runOctant
    runOctant octant = runReaderT (scan 0 0 1) (obstructionMap, squadius, octant)
    initialShadowMap = listArray mapBounds (repeat Hidden)

glueA2 :: Applicative f => (a -> b -> f c) -> (a -> b -> f d) -> (a -> b -> f d)
glueA2 a1 a2 x y = (a1 x y) *> (a2 x y)

from :: Delta -> Coord -> Coord -> [Coord]
from delta start end
  | start == end = [start]
  | otherwise = start:(from delta (addPoint start delta) end)

type ScanFolding a = (Slope, Maybe Opacity) -> (Opacity, Coord) -> Scanning a

scan :: Int -> Slope -> Slope -> Scanning ()
scan distance initialStartSlope initialEndSlope = do
  (obstructionMap, maxDistance, deltas@(majorDelta, minorDelta)) <- ask
  when (initialStartSlope <= initialEndSlope && distance <= maxDistance) $ do
    let pointAt slope = pointScaleF (fromIntegral distance * slope) majorDelta 
                        `addPoint`
                        pointScale distance minorDelta
    let spacesToLookAt = from majorDelta (pointAt initialStartSlope) (pointAt initialEndSlope)
    let visibilities = mapWith (obstructionMap !) spacesToLookAt
    let step = stepper deltas
    (newStartSlope, last) <- foldM (glueA2 reveal (preserving step)) (initialStartSlope, Nothing) visibilities
    when (last == Just Transparent) (scan (distance + 1) newStartSlope initialEndSlope)
  where
    reveal :: ScanFolding ()
    reveal (_, _) (_, coord) = tell (Dual [coord])
    preserving :: ScanFolding Slope -> ScanFolding (Slope, Maybe Opacity)
    preserving a1 args args2@(opacity, _) = (, Just opacity) <$> (a1 args args2)

    stepper :: (Delta, Delta) -> ScanFolding Slope
    stepper _ (startSlope, Nothing) _ = pure startSlope
    stepper _ (startSlope, Just o) (o', _) | o == o' = pure startSlope

    stepper deltas (startSlope, _) (Opaque, coord) = scan (distance + 1) startSlope newEndSlope $> startSlope
      where newEndSlope = slopeBetween deltas coord (before deltas coord)

    stepper deltas (startSlope, _) (Transparent, coord) = pure newStartSlope
      where newStartSlope = slopeBetween deltas coord (before deltas coord)

    before (majorDelta, minorDelta) coord = coord `subPoint` majorDelta

-- We calculate slope as majorAxis/minorAxis.
slopeBetween :: (Delta, Delta) -> Coord -> Coord -> Slope
slopeBetween ((xM, yM), (xm, ym)) (x, y) (x', y') = fromIntegral majors / fromIntegral minors
  where majors = (xM * x) + (yM * y) + (xM * x') + (yM * y')
        minors = (xm * x) + (ym * y) + (xm * x') + (ym * y')

mapWith :: (a -> b) -> [a] -> [(b, a)]
mapWith f [] = []
mapWith f (x:xs) = (f x, x):(mapWith f xs)
