{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import RoguePrelude
import Control.Lens (makeFields)

data Direction = North | South | East | West deriving (Eq, Show)

type Map = Array Coord Tile
type Coord = (Int, Int)
type Delta = (Int, Int)
type Rect = (Int, Int, Int, Int)
type MapSegment = (Rect, Map)
type Size = (Int, Int)

data Tile = Grass | Tree | Rock deriving (Show, Eq)

data Player = Player { _playerLocation :: Coord
                     , _playerSightRadius :: Int
                     , _playerHunger :: Int
                     , _playerFatigue :: Int
                     } deriving (Show, Eq)
$(makeFields ''Player)

data World = World { _worldPlayer :: Player
                   , _worldMap :: Map
                   , _worldTurn :: Int
                   } deriving (Show, Eq)
$(makeFields ''World)

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

lookupTile :: Coord -> MapSegment -> Maybe Tile
lookupTile (x, y) ((left, top, width, height), map)
  |    inRange (0, width - 1) x
    && inRange (0, height - 1) y
    && inRange (bounds map) globalPoint
    = Just (map ! globalPoint)
  | otherwise = Nothing
  where
    globalPoint = (x + left, y + top)

lookup :: Ix i => i -> Array i e -> Maybe e
lookup i a | inRange (bounds a) i = Just (a ! i)
           | otherwise = Nothing

globalToLocal :: Rect -> Coord -> Coord
globalToLocal (left, top, _, _) (x, y) = (x - left, y - top)
