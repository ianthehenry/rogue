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
import Control.Lens ((^.), over, to)

data Action = ActionCommand Command
            | ActionQuit

data Command = Move Direction

main :: IO ()
main = do
  vty <- mkVty def
  map <- randomMap
  play vty (World (Player (5, 5) 25 0 0) map 0)
  Vty.shutdown vty

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

play :: Vty -> World -> IO ()
play vty world = do
  updateDisplay vty world
  maybeAction <- parseEvent <$> Vty.nextEvent vty
  case maybeAction of
    Nothing -> again
    Just action -> case action of
      ActionQuit -> return ()
      ActionCommand c -> if canPerformCommand c world then
        play vty ((tick . performCommand c) world)
      else
        again
  where again = play vty world

tick :: World -> World
tick = over player tickPlayer . over turn succ

tickPlayer :: Player -> Player
tickPlayer = over hunger succ . over fatigue succ

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

drawWorld :: Rect -> World -> [Vty.Image]
drawWorld rect@(left, top, width, height) world = [infoImage, playerImage, mapImage]
  where
    infoImage = Vty.string Vty.defAttr ("Move with the arrows keys. Press q to exit. " ++ show playerPosition)
    playerImage = Vty.translate x y (Vty.char Vty.defAttr '@')
      where (x, y) = globalToLocal rect playerPosition
    mapImage = drawMap (rect, worldMap) localShadowMap
    localShadowMap = translateShadowMap (-left, -top) globalShadowMap
    globalShadowMap = makeShadowMap (world ^. player) worldMap
    worldMap = world ^. map
    playerPosition = world ^. player.location

makeShadowMap :: Player -> Map -> ShadowMap
makeShadowMap player map = translateShadowMap playerPosition clampedShadowMap
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

translateShadowMap :: Coord -> ShadowMap -> ShadowMap
translateShadowMap delta shadowMap = ixmap newBounds unoffset shadowMap
  where
    newBounds = mapBoth offset (bounds shadowMap)
    offset = addPoint delta
    unoffset = (`subPoint` delta)

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

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

type Size = (Int, Int)

messageLog :: Player -> Size -> Vty.Image
messageLog _ (width, height) = Vty.emptyImage

twoDigit :: Int -> String
twoDigit n
  | n < 10 = '0':d
  | n > 99 = undefined
  | otherwise = d
  where d = show n

worldInfo :: World -> Size -> Vty.Image
worldInfo world _ = Vty.string Vty.defAttr time
  where
    ticks = world ^. turn
    time = hour <> ":" <> minute <> " " <> meridiem
    minutes = ticks `div` 6
    minute = twoDigit (minutes `mod` 60)
    hours = minutes `div` 60

    hour = (twoDigit . zeroToTwelve) (hours `mod` 12)
    zeroToTwelve 0 = 12
    zeroToTwelve x = x
    meridiem = if (hours `mod` 24) < 12 then "AM" else "PM"

data HungerInterpretation = Full | Fine | Peckish | Hungry | Starving | Ravenous deriving (Show)

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

data FatigueInterpretation = Spritely | Awake | Weary | Tired | Exhausted | BarelyAwake
instance Show FatigueInterpretation where
  show Spritely = "Spritely"
  show Awake = "Awake"
  show Weary = "Weary"
  show Tired = "Tired"
  show Exhausted = "Exhausted"
  show BarelyAwake = "About to Drop"

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

playerInfo :: Player -> Size -> Vty.Image
playerInfo player (width, height) = Vty.vertCat $ fmap (Vty.string Vty.defAttr)
  [ "Hunger: " <> (player ^. hungerString)
  , "Fatigue: " <> (player ^. fatigueString)
  ]
  where
    hungerString = hunger . to interpretHunger . to show
    fatigueString = fatigue . to interpretFatigue . to show

stack :: [(Size -> Vty.Image)] -> Size -> Vty.Image
stack fs (totalWidth, totalHeight)
  | heightEach <= 0 = undefined
  | otherwise = Vty.vertCat (intersperse border components)
  where
    border = Vty.charFill Vty.defAttr '-' totalWidth 1
    components = fmap ($ sizeEach) fs
    borders = (length fs) - 1
    heightEach = (totalHeight - borders) `div` (length fs)
    sizeEach = (totalWidth, heightEach)

translateLayers :: Int -> Int -> Vty.Picture -> [Vty.Image]
translateLayers x y Vty.Picture { Vty.picLayers = layers } = (Vty.translate x y) <$> layers

updateDisplay :: Vty -> World -> IO ()
updateDisplay vty world = do
  (screenWidth, screenHeight) <- Vty.displayBounds (Vty.outputIface vty)
  let mapWidth = max 40 (screenWidth `div` 2)
  let menuWidth = screenWidth - mapWidth - borderWidth
  let viewport = rectCenteredAt playerPosition (mapWidth, screenHeight)

  let menu = makeMenu (menuWidth, screenHeight)
  let border = Vty.charFill Vty.defAttr '|' borderWidth screenHeight
  let menuImage = Vty.horizCat [menu, border]
  let mapLayers = (Vty.translate (menuWidth + borderWidth) 0) <$> drawWorld viewport world
  let picture = Vty.picForLayers (menuImage:mapLayers)
  Vty.update vty picture
  where
    you = world ^. player
    borderWidth = 1
    playerPosition = you ^. location
    makeMenu = stack [worldInfo world, playerInfo you, messageLog you]

rectCenteredAt :: (Int, Int) -> (Int, Int) -> Rect
rectCenteredAt (x, y) (width, height) = (x - width `div` 2, y - height `div` 2, width, height)

squareCenteredAt :: (Int, Int) -> Int -> Rect
squareCenteredAt (x, y) squadius = (x - squadius, y - squadius, 2 * squadius + 1, 2 * squadius + 1)

charForTile :: Tile -> Char
charForTile Rock = '#'
charForTile Tree = '♣'
charForTile Grass = '·'

drawMap :: MapSegment -> ShadowMap -> Vty.Image
drawMap segment@((_, _, width, height), map) shadowMap = (Vty.vertCat . fmap rowImage) (range (0, height))
  where
    rowImage y = (Vty.horizCat . fmap (Vty.translateX 0 . imageAt)) row
      where row = range ((0, y), (width, y))
    imageAt coord | isVisible coord = Vty.char Vty.defAttr (charAt coord)
                  | otherwise = Vty.char Vty.defAttr ' '
    charAt coord = maybe '^' charForTile (lookupTile coord segment)
    isVisible coord = inRange (bounds shadowMap) coord && (shadowMap ! coord) == Visible
