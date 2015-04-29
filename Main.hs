module Main where

import BasePrelude hiding (map, lookup)
import Graphics.Vty (Vty, mkVty)
import qualified Graphics.Vty as Vty
import Data.Array
import Data.Default (def)
import System.Random
import Control.Monad.State (State, modify, execState, get)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Types
import FOV
import Control.Lens ((^.), over)

data Action = ActionCommand Command
            | ActionQuit

data Command = Move Direction

main :: IO ()
main = do
  vty <- mkVty def
  world <- World (Player (5, 5)) <$> randomMap
  play vty world
  Vty.shutdown vty

randomMap :: IO Map
randomMap = do
  let geoRange = ((0, 0), (100, 100))
  pieces <- replicateM (rangeSize geoRange) randomPiece
  return (listArray geoRange pieces)
  where
    randomPiece :: IO Tile
    randomPiece = toTile <$> randomRIO (0, 100)
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
  inRange (bounds worldMap) destination &&
  opacity (worldMap ! destination) == Transparent
  where
    worldMap = world ^. map
    destination = move dir (world ^. player.location)

lookup :: Ix i => i -> Array i e -> Maybe e
lookup i a | inRange (bounds a) i = Just (a ! i)
           | otherwise = Nothing

performCommand :: Command -> World -> World
performCommand (Move dir) = over (player.location) (move dir)

drawWorld :: Rect -> World -> Vty.Picture
drawWorld rect@(left, top, width, height) world = Vty.picForLayers [infoImage, playerImage, mapImage]
  where
    infoImage = Vty.string Vty.defAttr ("Move with the arrows keys. Press q to exit. " ++ show playerPosition)
    playerImage = Vty.translate (x * 2 + 1) y (Vty.char Vty.defAttr '@')
      where (x, y) = globalToLocal rect playerPosition
    mapImage = drawMap (rect, worldMap) translatedShadowMap

    worldMap = world ^. map
    playerPosition = world ^. player.location

    obstructionMap = makeObstructionMap playerPosition 25 worldMap
    shadowMap = fieldOfView obstructionMap
    clampedShadowMap = mapArray (clampCircle 25) shadowMap
    translatedShadowMap = translateShadowMap (clampedShadowMap, playerPosition) rect

clampCircle ::Int -> Coord -> Visibility -> Visibility
clampCircle _ _ Hidden = Hidden
clampCircle radius (x, y) Visible
  | (x * x + y * y) > (radius * radius) = Hidden
  | otherwise = Visible

mapArray :: Ix i => (i -> a -> b) -> Array i a -> Array i b
mapArray f a = (listArray (bounds a) . fmap (uncurry f) . assocs) a

opacity :: Tile -> Opacity
opacity Rock = Opaque
opacity Tree = Opaque
opacity _ = Transparent

makeObstructionMap :: Coord -> Int -> Map -> ObstructionMap
makeObstructionMap center viewDistance map = array bounds' obstructions
  where
    bounds' = ((-viewDistance, -viewDistance), (viewDistance, viewDistance))
    obstructions = do
      local <- range bounds'
      let global = localToGlobal local
      return (local, maybe Opaque opacity (lookup global map))
    localToGlobal = addPoint center

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
  (width, height) <- Vty.displayBounds (Vty.outputIface vty)
  let playerPosition = world ^. player.location
  let viewport = rectCenteredAt playerPosition (width `div` 2, height)
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
drawMap segment@((_, _, width, height), map) shadowMap = (Vty.vertCat . fmap rowImage) (range (0, height))
  where
    rowImage y = (Vty.horizCat . fmap (Vty.translateX 1 . imageAt)) row
      where row = range ((0, y), (width, y))
    imageAt coord | isVisible coord = Vty.char Vty.defAttr (charAt coord)
                  | otherwise = Vty.char Vty.defAttr ' '
    charAt coord = maybe '^' charForTile (lookupTile coord segment)
    isVisible coord = inRange (bounds shadowMap) coord && (shadowMap ! coord) == Visible
