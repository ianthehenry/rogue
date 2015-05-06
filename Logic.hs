module Logic where

import RoguePrelude
import Types
import Control.Monad.Random
import Control.Lens ((^.), over)
import FOV

data Action = ActionCommand Command
            | ActionQuit

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

type Ticker = Rand StdGen

tick :: World -> Ticker World
tick =  pure . over turn succ
    >=> player tickPlayer
    >=> mobs (traverse wiggle)

randomDirection :: Ticker Direction
randomDirection = do
  i <- getRandomR (0, 3 :: Int)
  pure $ case i of
    0 -> South
    1 -> West
    2 -> East
    3 -> North

wiggle :: Mob -> Ticker Mob
wiggle mob = do
  direction <- randomDirection
  pure (over location (move direction) mob)

tickPlayer :: Player -> Ticker Player
tickPlayer =  pure . over hunger succ
          >=> pure . over fatigue succ

move :: Direction -> Coord -> Coord
move North (x, y) = (x, y - 1)
move South (x, y) = (x, y + 1)
move East  (x, y) = (x + 1, y)
move West  (x, y) = (x - 1, y)

canPerformCommand :: Command -> World -> Bool
canPerformCommand (Move dir) world =
  inRange (bounds worldMap) destination &&
  opacity (worldMap ! destination) == Transparent
  where
    worldMap = world ^. map
    destination = move dir (world ^. player.location)

performCommand :: Command -> World -> World
performCommand (Move dir) = over (player.location) (move dir)

makeShadowMap :: Player -> Map -> ShadowMap
makeShadowMap player map = translateMap playerPosition clampedShadowMap
  where
    playerPosition = player ^. location
    viewDistance = player ^. sightRadius
    obstructionMap = makeObstructionMap playerPosition viewDistance map
    shadowMap = fieldOfView obstructionMap
    clampedShadowMap = mapArray (clampCircle viewDistance) shadowMap

clampCircle :: Int -> Coord -> Visibility -> Visibility
clampCircle _ _ Hidden = Hidden
clampCircle radius (x, y) Visible
  | (x * x + y * y) > (radius * radius) = Hidden
  | otherwise = Visible

mapArray :: Ix i => (i -> a -> b) -> Array i a -> Array i b
mapArray f a = (listArray (bounds a) . fmap (uncurry f) . assocs) a

makeObstructionMap :: Coord -> Int -> Map -> ObstructionMap
makeObstructionMap center viewDistance map = array bounds' obstructions
  where
    bounds' = ((-viewDistance, -viewDistance), (viewDistance, viewDistance))
    obstructions = do
      local <- range bounds'
      let global = localToGlobal local
      return (local, maybe Opaque opacity (lookup global map))
    localToGlobal = addPoint center

translateMap :: Coord -> (Array Coord a) -> (Array Coord a)
translateMap delta map = ixmap newBounds unoffset map
  where
    newBounds = mapBoth offset (bounds map)
    offset = addPoint delta
    unoffset = (`subPoint` delta)
    mapBoth f (x, y) = (f x, f y)
