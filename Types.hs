{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import RoguePrelude
import Control.Lens

data Direction = North | South | East | West deriving (Eq, Show)

type Topo = Array Coord Tile
type Coord = (Int, Int)
type Delta = (Int, Int)
type Rect = (Int, Int, Int, Int)
type TopoSegment = (Rect, Topo)
type Size = (Int, Int)

data Tile = Grass | Tree | Rock deriving (Show, Eq)

data Player = Player { _playerLocation :: Coord
                     , _playerSightRadius :: Int
                     , _playerHunger :: Int
                     , _playerFatigue :: Int
                     }
$(makeFields ''Player)

data MobSpecies = Zombie
type Id = Int

type Identified a = (Id, a)

hasId :: Id -> (Id, a) -> Bool
hasId x (y, _) = x == y

data Mob = Mob { _mobLocation :: Coord
               , _mobHealth :: Int
               , _mobSpecies :: MobSpecies
               }
$(makeFields ''Mob)

data World = World { _worldPlayer :: Player
                   , _worldTopo :: Topo
                   , _worldTurn :: Int
                   , _worldNextId :: Int
                   , _worldMobs :: [Identified Mob]
                   }
$(makeFields ''World)

makeWorld :: Topo -> Player -> [Mob] -> World
makeWorld topo player mobs = foldr ($) emptyWorld (addMob <$> mobs)
  where emptyWorld = (World player topo 0 0 [])

addMob :: Mob -> World -> World
addMob mob world = world & mobs %~ (newMob :)
                         & nextId %~ succ
  where newMob = (world ^. nextId, mob)

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

lookupTile :: Coord -> TopoSegment -> Maybe Tile
lookupTile (x, y) ((left, top, width, height), topo)
  |    inRange (0, width - 1) x
    && inRange (0, height - 1) y
    && inRange (bounds topo) globalPoint
    = Just (topo ! globalPoint)
  | otherwise = Nothing
  where
    globalPoint = (x + left, y + top)

lookup :: Ix i => i -> Array i e -> Maybe e
lookup i a | inRange (bounds a) i = Just (a ! i)
           | otherwise = Nothing

globalToLocal :: Rect -> Coord -> Coord
globalToLocal (left, top, _, _) (x, y) = (x - left, y - top)

containsCoord :: Rect -> Coord -> Bool
containsCoord (left, top, width, height) (x, y) = inRange (left, left + width) x
                                               && inRange (top, top + height) y
