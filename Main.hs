module Main where

import RoguePrelude
import Data.Default (def)
import Graphics.Vty (mkVty, shutdown)
import System.Random
import Types
import UI

main :: IO ()
main = do
  vty <- mkVty def
  map <- randomMap
  play vty (makeWorld map player mobs)
  shutdown vty
  where
    player = Player (5, 5) 25 0 0
    mobs = [Mob (10, 10) 100 Zombie]

randomMap :: IO Map
randomMap = do
  pieces <- replicateM (rangeSize mapBounds) randomPiece
  return (listArray mapBounds pieces)
  where
    mapBounds = ((0, 0), (1000, 1000))
    randomPiece :: IO Tile
    randomPiece = toTile <$> randomRIO (0, 100)
    toTile :: Int -> Tile
    toTile 0 = Rock
    toTile 1 = Tree
    toTile 2 = Tree
    toTile _ = Grass
