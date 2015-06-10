{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import RoguePrelude
import Control.Lens
import Control.Lens.Indexed
import Data.Map (Map)
import qualified Data.Map as Map

data Direction = North | South | East | West deriving (Eq, Show)

type Topo = Array Coord Tile
type Coord = (Int, Int)
type Delta = (Int, Int)
type Rect = (Int, Int, Int, Int)
type TopoSegment = (Rect, Topo)
type Size = (Int, Int)

data Tile = Grass | Tree | Rock deriving (Show, Eq)

data Species = Human | Zombie

data Actor = Actor { _actorLocation :: Coord
                   , _actorSightRadius :: Int
                   , _actorHunger :: Int
                   , _actorFatigue :: Int
                   , _actorSpecies :: Species
                   }
$(makeFields ''Actor)

type Id = Int

hasId :: Id -> (Id, a) -> Bool
hasId x (y, _) = x == y

data World = World { _worldTopo :: Topo
                   , _worldTurn :: Int
                   , _worldNextId :: Int
                   , _worldActors :: Map Id Actor
                   }
$(makeFields ''World)

actorWithId :: Id -> Lens' World Actor
actorWithId id = actors.at' id

player :: Lens' World Actor
player = actorWithId 0

at' :: Ord k => k -> Lens' (Map k a) a
at' k f m = f v <&> \v' -> Map.insert k v' m
  where v = (Map.!) m k

makeWorld :: Topo -> [Actor] -> World
makeWorld topo actors = runAll (addActor <$> actors) emptyWorld
  where emptyWorld = (World topo 0 0 Map.empty)

runAll :: [(a -> a)] -> a -> a
runAll fns state = foldl' (flip ($)) state fns

addActor :: Actor -> World -> World
addActor actor world = world & actors %~ (Map.insert (world ^. nextId) actor)
                             & nextId %~ succ

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
