module Main where

import BasePrelude hiding (map)
import Graphics.Vty (Vty, mkVty)
import qualified Graphics.Vty as Vty
import Data.Array
import Data.Default (def)
import System.Random

data Player = Player { playerCoord :: Coord
                     } deriving (Show, Eq)

data World = World { player :: Player
                   , worldMap :: Map
                   } deriving (Show, Eq)

data Tile = EmptySpace
          | Rock
          | Grass
          | TallGrass
          | OtherGrass
          deriving (Show, Eq, Enum, Bounded)

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
  world <- World (Player (10, 10)) <$> randomMap
  play vty world
  Vty.shutdown vty

randomMap :: IO Map
randomMap = do
  let geoRange = ((0, 0), (79, 79))
  pieces <- replicateM (rangeSize geoRange) randomPiece
  return (listArray geoRange pieces)
  where
    randomPiece :: IO Tile
    randomPiece = toEnum <$> randomRIO (fromEnum min, fromEnum max)
    (min, max) = (minBound, maxBound) :: (Tile, Tile)

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
  (worldMap world) ! destination /= Rock
  where destination = move dir (playerCoord (player world))

performCommand :: Command -> World -> World
performCommand (Move dir) world@World { player = Player pos } =
  world { player = Player (move dir pos) }

drawWorld :: Rect -> World -> Vty.Picture
drawWorld rect@(left, top, width, height) world = Vty.picForLayers [info, offset playerImage, mapImage]
  where
    offset = Vty.translate (-left) (-top)
    info = Vty.string Vty.defAttr "Move with the arrows keys. Press q to exit."
    mapImage = drawMap rect (worldMap world)
    playerImage = Vty.translate x y (Vty.char Vty.defAttr '@')
    (x, y) = (playerCoord . player) world

updateDisplay :: Vty -> World -> IO ()
updateDisplay vty world = do
  displaySize <- Vty.displayBounds (Vty.outputIface vty)
  let playerPosition = (playerCoord . player) world
  let viewport = rectCenteredAt playerPosition displaySize
  let picture = drawWorld viewport world
  Vty.update vty picture

rectCenteredAt :: (Int, Int) -> (Int, Int) -> Rect
rectCenteredAt (x, y) (width, height) = (x - width `div` 2, y - height `div` 2, width, height)

charForTile :: Tile -> Char
charForTile EmptySpace = ' '
charForTile Rock = '▮'
charForTile Grass = ','
charForTile TallGrass = '\''
charForTile OtherGrass = '`'

drawMap :: Rect -> Map -> Vty.Image
drawMap (left, top, width, height) map = Vty.vertCat (row <$> (range (startY, endY)))
  where
    row y = Vty.horizCat (imageAt <$> (range ((startX, y), (endX, y))))
    (startX, startY, endX, endY) = (left, top, left + width, top + height)
    imageAt = Vty.char Vty.defAttr . safeLookup
    safeLookup ix = if inRange (bounds map) ix then (charForTile (map ! ix)) else ' '
