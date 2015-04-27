module Main where

import BasePrelude hiding (map, lookup)
import Graphics.Vty (Vty, mkVty)
import qualified Graphics.Vty as Vty
import Data.Array
import Data.Default (def)
import System.Random
import Control.Monad.State (State, modify, execState, get)
import Control.Monad.Reader (ReaderT, ask, runReaderT)

data Player = Player { playerCoord :: Coord
                     } deriving (Show, Eq)

data World = World { player :: Player
                   , worldMap :: Map
                   } deriving (Show, Eq)

data Tile = Grass | Tree | Rock deriving (Show, Eq)

data Action = ActionCommand Command
            | ActionQuit

data Command = Move Direction

data Direction = North | South | East | West deriving (Eq, Show)

type Map = Array Coord Tile
type Coord = (Int, Int)
type Rect = (Int, Int, Int, Int)

main :: IO ()
main = do
  vty <- mkVty def
  world <- World (Player (5, 5)) <$> randomMap
  play vty world
  Vty.shutdown vty

randomMap :: IO Map
randomMap = do
  let geoRange = ((0, 0), (79, 79))
  pieces <- replicateM (rangeSize geoRange) randomPiece
  return (listArray geoRange pieces)
  where
    randomPiece :: IO Tile
    randomPiece = toTile <$> randomRIO (0, 50)
    toTile :: Int -> Tile
    toTile 0 = Rock
    toTile 1 = Tree
    toTile 2 = Tree
    toTile _ = Grass

play :: Vty -> World -> IO ()
play vty world = do
  updateDisplay vty world
  maybeAction <- parseEvent <$> Vty.nextEvent vty
  case maybeAction of
    Nothing -> again
    Just action -> case action of
      ActionQuit -> return ()
      ActionCommand c -> if canPerformCommand c world then
        play vty (performCommand c world)
      else
        again
  where again = play vty world

move :: Direction -> Coord -> Coord
move North (x, y) = (x, y - 1)
move South (x, y) = (x, y + 1)
move East  (x, y) = (x + 1, y)
move West  (x, y) = (x - 1, y)

c :: Command -> Maybe Action
c = Just . ActionCommand

parseEvent :: Vty.Event -> Maybe Action
parseEvent (Vty.EvKey Vty.KUp [])    = c (Move North)
parseEvent (Vty.EvKey Vty.KDown [])  = c (Move South)
parseEvent (Vty.EvKey Vty.KRight []) = c (Move East)
parseEvent (Vty.EvKey Vty.KLeft [])  = c (Move West)
parseEvent (Vty.EvKey (Vty.KChar 'q') []) = Just ActionQuit
parseEvent _ = Nothing

canPerformCommand :: Command -> World -> Bool
canPerformCommand (Move dir) world =
  inRange (bounds (worldMap world)) destination &&
  opacity ((worldMap world) ! destination) == Transparent
  where destination = move dir (playerCoord (player world))

type ShadowMap = Array Coord Visibility
type ObstructionMap = Array Coord Opacity

data Visibility = Visible | Hidden deriving (Eq, Show)
data Opacity = Transparent | Opaque deriving (Eq)

type Delta = (Int, Int)
type ScanInfo = (ObstructionMap, Int, (Delta, Delta))
type Scanning = ReaderT ScanInfo (State ShadowMap)
type Slope = Rational

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

pointJoin :: (Int -> Int -> Int) -> Coord -> Delta -> Coord
pointJoin f (x, y) (x', y') = (f x x', f y y')

addPoint :: Coord -> Delta -> Coord
addPoint = pointJoin (+)

subPoint :: Coord -> Delta -> Coord
subPoint = pointJoin (-)

pointScaleF :: Rational -> Delta -> Delta
pointScaleF m (x, y) = (round (fromIntegral x * m), round (fromIntegral y * m))

pointScale :: Int -> Delta -> Delta
pointScale m (x, y) = (x * m, y * m)

mapWith :: (a -> b) -> [a] -> [(b, a)]
mapWith f [] = []
mapWith f (x:xs) = (f x, x):(mapWith f xs)

performCommand :: Command -> World -> World
performCommand (Move dir) world@World { player = Player pos } =
  world { player = Player (move dir pos) }

drawWorld :: Rect -> World -> Vty.Picture
drawWorld rect@(left, top, width, height) world = Vty.picForLayers [info, playerImage, mapImage]
  where
    info = Vty.string Vty.defAttr ("Move with the arrows keys. Press q to exit. " ++ show playerPosition)
    playerImage = let (x, y) = globalToLocal rect playerPosition in Vty.translate x y (Vty.char Vty.defAttr '@')
    mapImage = drawMap (rect, map) shadowMap

    map = worldMap world
    playerPosition = (playerCoord . player) world
    shadowMap = translateShadowMap (fieldOfView map playerPosition 25, playerPosition) rect

globalToLocal :: Rect -> Coord -> Coord
globalToLocal (left, top, _, _) (x, y) = (x - left, y - top)

translateShadowMap :: (ShadowMap, Coord) -> Rect -> ShadowMap
translateShadowMap (shadowMap, shadowMapCenter) (left, top, _, _) =
  ixmap newBounds unoffset shadowMap
  where
    newBounds@(newStart, _) = (offset prevStart, offset prevEnd)
    offset = addPoint (x - left, y - top)

    deltaStart = prevStart `subPoint` newStart
    (prevStart, prevEnd) = bounds shadowMap
    unoffset = addPoint deltaStart
    (x, y) = shadowMapCenter

type MapSegment = (Rect, Map)

lookupTile :: Coord -> MapSegment -> Maybe Tile
lookupTile (x, y) ((left, top, width, height), map)
  |    inRange (0, width - 1) x
    && inRange (0, height - 1) y
    && inRange (bounds map) globalPoint
    = Just (map ! globalPoint)
  | otherwise = Nothing
  where
    globalPoint = (x + left, y + top)

updateDisplay :: Vty -> World -> IO ()
updateDisplay vty world = do
  displaySize <- Vty.displayBounds (Vty.outputIface vty)
  let playerPosition = (playerCoord . player) world
  let viewport = rectCenteredAt playerPosition displaySize
  let picture = drawWorld viewport world
  Vty.update vty picture

rectCenteredAt :: (Int, Int) -> (Int, Int) -> Rect
rectCenteredAt (x, y) (width, height) = (x - width `div` 2, y - height `div` 2, width, height)

squareCenteredAt :: (Int, Int) -> Int -> Rect
squareCenteredAt (x, y) squadius = (x - squadius, y - squadius, 2 * squadius + 1, 2 * squadius + 1)

charForTile :: Tile -> Char
charForTile Rock = '#'
charForTile Tree = '♣'
charForTile Grass = '.'

drawMap :: MapSegment -> ShadowMap -> Vty.Image
drawMap segment@((_, _, width, height), map) shadowMap = Vty.vertCat (row <$> (range (0, height)))
  where
    row y = Vty.horizCat (imageAt <$> range ((0, y), (width, y)))
    imageAt coord | isVisible coord = Vty.char Vty.defAttr (charAt coord)
                  | otherwise = Vty.char Vty.defAttr ' '
    charAt coord = maybe '^' charForTile (lookupTile coord segment)
    isVisible coord = inRange (bounds shadowMap) coord && (shadowMap ! coord) == Visible
