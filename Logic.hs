{-# LANGUAGE RankNTypes #-}

module Logic where

import RoguePrelude
import Types
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Random
import Control.Lens
import FOV

data Command = Move Direction

data HungerInterpretation = Full | Fine | Peckish | Hungry | Starving | Ravenous deriving (Show)
data FatigueInterpretation = Spritely | Awake | Weary | Tired | Exhausted | BarelyAwake

opacity :: Tile -> Opacity
opacity Rock = Opaque
opacity Tree = Opaque
opacity _ = Transparent

interpretHunger :: Int -> HungerInterpretation
interpretHunger n
  | n > hours 24 = Ravenous
  | n > hours 16 = Starving
  | n > hours 8  = Hungry
  | n > hours 4  = Peckish
  | n > 6 * 30   = Fine
  | otherwise    = Full
  where
    hours = (6 * 60 *)

interpretFatigue :: Int -> FatigueInterpretation
interpretFatigue n
  | n > hours 24 = BarelyAwake
  | n > hours 16 = Exhausted
  | n > hours 14 = Tired
  | n > hours 12 = Weary
  | n > hours 1  = Awake
  | otherwise    = Spritely
  where
    hours = (6 * 60 *)

type Ticker a = forall m. MonadRandom m => m a

tick :: World -> Ticker World
tick =  pure . over turn succ
    >=> player tickPlayer
    >=> (actors.traverse) tickActor

randomDirection :: MonadRandom m => m Direction
randomDirection = do
  i <- getRandomR (0, 3 :: Int)
  pure $ case i of
    0 -> South
    1 -> West
    2 -> East
    3 -> North

tickActor :: Actor -> Ticker Actor
tickActor = pure

zmap :: (a -> b) -> [a] -> [(a, b)]
zmap f xs = zip xs (fmap f xs)

tickPlayer :: Actor -> Ticker Actor
tickPlayer =  pure . over hunger succ
          >=> pure . over fatigue succ

move :: Direction -> Coord -> Coord
move North (x, y) = (x, y - 1)
move South (x, y) = (x, y + 1)
move East  (x, y) = (x + 1, y)
move West  (x, y) = (x - 1, y)

canPerformCommand :: HasLocation a Coord => a -> Command -> World -> Bool
canPerformCommand actor (Move dir) world =
  inRange (bounds worldTopo) destination &&
  opacity (worldTopo ! destination) == Transparent
  where
    worldTopo = world ^. topo
    destination = move dir (actor ^. location)

performCommandIfPossible :: (Id, Command) -> World -> World
performCommandIfPossible (id, command) world =
  if canPerformCommand actor command world then
    performCommand (id, command) world
  else
    world
  where actor = world ^. actorWithId id

performCommand :: (Id, Command) -> World -> World
performCommand (id, (Move dir)) world =
  ( change location (move dir)
  . change nextTurn (+ speed)
  ) world
  where
    actor :: Lens' World Actor
    actor = actorWithId id
    change lens = over (actor.lens)
    speed = case world ^. actor.species of
      Human -> 10
      Zombie -> 12

makeShadowTopo :: Actor -> Topo -> ShadowTopo
makeShadowTopo actor topo = translateTopo actorPosition clampedShadowTopo
  where
    actorPosition = actor ^. location
    viewDistance = actor ^. sightRadius
    obstructionTopo = makeObstructionTopo actorPosition viewDistance topo
    shadowTopo = fieldOfView obstructionTopo
    clampedShadowTopo = mapArray (clampCircle viewDistance) shadowTopo

clampCircle :: Int -> Coord -> Visibility -> Visibility
clampCircle _ _ Hidden = Hidden
clampCircle radius (x, y) Visible
  | (x * x + y * y) > (radius * radius) = Hidden
  | otherwise = Visible

mapArray :: Ix i => (i -> a -> b) -> Array i a -> Array i b
mapArray f a = (listArray (bounds a) . fmap (uncurry f) . assocs) a

makeObstructionTopo :: Coord -> Int -> Topo -> ObstructionTopo
makeObstructionTopo center viewDistance topo = array bounds' obstructions
  where
    bounds' = ((-viewDistance, -viewDistance), (viewDistance, viewDistance))
    obstructions = do
      local <- range bounds'
      let global = localToGlobal local
      return (local, maybe Opaque opacity (lookup global topo))
    localToGlobal = addPoint center

translateTopo :: Coord -> (Array Coord a) -> (Array Coord a)
translateTopo delta topo = ixmap newBounds unoffset topo
  where
    newBounds = mapBoth offset (bounds topo)
    offset = addPoint delta
    unoffset = (`subPoint` delta)
    mapBoth f (x, y) = (f x, f y)
