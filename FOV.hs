module FOV
( fieldOfView
, ShadowMap
, Visibility(..)
, Opacity(..)
, opacity
) where

import BasePrelude hiding (map, lookup)
import Types
import Data.Array
import Control.Monad.State (State, modify, execState, get)
import Control.Monad.Reader (ReaderT, ask, runReaderT)

type ScanInfo = (ObstructionMap, Int, (Delta, Delta))
type Scanning = ReaderT ScanInfo (State ShadowMap)
type Slope = Rational

type ShadowMap = Array Coord Visibility
type ObstructionMap = Array Coord Opacity

data Visibility = Visible | Hidden deriving (Eq, Show)
data Opacity = Transparent | Opaque deriving (Eq)

opacity :: Tile -> Opacity
opacity Rock = Opaque
opacity Tree = Opaque
opacity _ = Transparent

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

fieldOfView :: Map -> Coord -> Int -> ShadowMap
fieldOfView map (centerX, centerY) squadius = execState runOctants initialShadowMap
  where
    runOctants = forM_ octants runOctant
    runOctant octant = runReaderT (scan 1 0 1) (argsFor octant)
    argsFor = (obstructionMap, squadius,)
    bounds = ((-squadius, -squadius), (squadius, squadius))
    initialShadowMap = listArray bounds (repeat Hidden)
    obstructionMap = array bounds obstructions
    obstructions = do
      local <- range bounds
      let global = localToGlobal local
      return (local, maybe Opaque opacity (lookup global map))
    localToGlobal (x, y) = (x + centerX, y + centerY)

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
    reveal (_, _) (_, coord) = modify (// [(coord, Visible)])
    preserving :: ScanFolding Slope -> ScanFolding (Slope, Maybe Opacity)
    preserving a1 args args2@(opacity, _) = (, Just opacity) <$> (a1 args args2)

    stepper :: (Delta, Delta) -> ScanFolding Slope
    stepper _ (startSlope, Nothing) _ = pure startSlope
    stepper _ (startSlope, Just o) (o', _) | o == o' = pure startSlope

    stepper deltas (startSlope, _) (Opaque, coord) = scan (distance + 1) startSlope newEndSlope $> startSlope
      where newEndSlope = slopeBetween deltas coord (before deltas coord)

    stepper deltas (startSlope, _) (Transparent, coord) = pure newStartSlope
      where newStartSlope = slopeBetween deltas coord (afterPrevious deltas coord)

    before, afterPrevious :: (Delta, Delta) -> Coord -> Coord
    before (majorDelta, minorDelta) coord = (coord `addPoint` minorDelta) `subPoint` majorDelta
    afterPrevious (majorDelta, minorDelta) coord = (coord `subPoint` majorDelta) `subPoint` minorDelta

-- We calculate slope as majorAxis/minorAxis.
slopeBetween :: (Delta, Delta) -> Coord -> Coord -> Slope
slopeBetween ((xM, yM), (xm, ym)) (x, y) (x', y') = fromIntegral majors / fromIntegral minors
  where majors = (xM * x) + (yM * y) + (xM * x') + (yM * y')
        minors = (xm * x) + (ym * y) + (xm * x') + (ym * y')

mapWith :: (a -> b) -> [a] -> [(b, a)]
mapWith f [] = []
mapWith f (x:xs) = (f x, x):(mapWith f xs)
