module Types where

import BasePrelude
import Data.Array (Array)

data Direction = North | South | East | West deriving (Eq, Show)

type Map = Array Coord Tile
type Coord = (Int, Int)
type Delta = (Int, Int)
type Rect = (Int, Int, Int, Int)

data Tile = Grass | Tree | Rock deriving (Show, Eq)

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
