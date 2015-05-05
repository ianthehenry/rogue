module UI (play) where

import Graphics.Vty
import RoguePrelude
import Types
import Logic
import FOV (Visibility(..), ShadowMap)
import Control.Lens ((^.), to)

play :: Vty -> World -> IO ()
play vty world = do
  updateDisplay vty world
  maybeAction <- parseEvent <$> nextEvent vty
  case maybeAction of
    Nothing -> again
    Just action -> case action of
      ActionQuit -> return ()
      ActionCommand c -> if canPerformCommand c world then
        play vty ((tick . performCommand c) world)
      else
        again
  where again = play vty world

parseEvent :: Event -> Maybe Action
parseEvent (EvKey KUp [])    = c (Move North)
parseEvent (EvKey KDown [])  = c (Move South)
parseEvent (EvKey KRight []) = c (Move East)
parseEvent (EvKey KLeft [])  = c (Move West)
parseEvent (EvKey (KChar 'q') []) = Just ActionQuit
parseEvent _ = Nothing

c :: Command -> Maybe Action
c = Just . ActionCommand

drawWorld :: Rect -> World -> [Image]
drawWorld rect@(left, top, width, height) world = [infoImage, playerImage, mapImage]
  where
    infoImage = string defAttr ("Move with the arrows keys. Press q to exit. " ++ show playerPosition)
    playerImage = translate x y (char defAttr '@')
      where (x, y) = globalToLocal rect playerPosition
    globalToLocal (left, top, _, _) (x, y) = (x - left, y - top)
    mapImage = drawMap (rect, worldMap) localShadowMap
    localShadowMap = translateMap (-left, -top) globalShadowMap
    globalShadowMap = makeShadowMap (world ^. player) worldMap
    worldMap = world ^. map
    playerPosition = world ^. player.location

messageLog :: Player -> Size -> Image
messageLog _ (width, height) = emptyImage

twoDigit :: Int -> String
twoDigit n
  | n < 10 = '0':d
  | n > 99 = undefined
  | otherwise = d
  where d = show n

worldInfo :: World -> Size -> Image
worldInfo world _ = string defAttr time
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

instance Show FatigueInterpretation where
  show Spritely = "Spritely"
  show Awake = "Awake"
  show Weary = "Weary"
  show Tired = "Tired"
  show Exhausted = "Exhausted"
  show BarelyAwake = "About to Drop"

playerInfo :: Player -> Size -> Image
playerInfo player (width, height) = vertCat $ fmap (string defAttr)
  [ "Hunger: " <> (player ^. hungerString)
  , "Fatigue: " <> (player ^. fatigueString)
  ]
  where
    hungerString = hunger . to interpretHunger . to show
    fatigueString = fatigue . to interpretFatigue . to show

stack :: [(Size -> Image)] -> Size -> Image
stack fs (totalWidth, totalHeight)
  | heightEach <= 0 = undefined
  | otherwise = vertCat (intersperse border components)
  where
    border = charFill defAttr '-' totalWidth 1
    components = fmap ($ sizeEach) fs
    borders = (length fs) - 1
    heightEach = (totalHeight - borders) `div` (length fs)
    sizeEach = (totalWidth, heightEach)

translateLayers :: Int -> Int -> Picture -> [Image]
translateLayers x y Picture { picLayers = layers } = (translate x y) <$> layers

updateDisplay :: Vty -> World -> IO ()
updateDisplay vty world = do
  (screenWidth, screenHeight) <- displayBounds (outputIface vty)
  let mapWidth = max 40 (screenWidth `div` 2)
  let menuWidth = screenWidth - mapWidth - borderWidth
  let viewport = rectCenteredAt playerPosition (mapWidth, screenHeight)

  let menu = makeMenu (menuWidth, screenHeight)
  let border = charFill defAttr '|' borderWidth screenHeight
  let menuImage = horizCat [menu, border]
  let mapLayers = (translate (menuWidth + borderWidth) 0) <$> drawWorld viewport world
  let picture = picForLayers (menuImage:mapLayers)
  update vty picture
  where
    you = world ^. player
    borderWidth = 1
    playerPosition = you ^. location
    makeMenu = stack [worldInfo world, playerInfo you, messageLog you]

rectCenteredAt :: Coord -> Size -> Rect
rectCenteredAt (x, y) (width, height) = (x - width `div` 2, y - height `div` 2, width, height)

squareCenteredAt :: Coord -> Int -> Rect
squareCenteredAt (x, y) squadius = (x - squadius, y - squadius, 2 * squadius + 1, 2 * squadius + 1)

charForTile :: Tile -> Char
charForTile Rock = '#'
charForTile Tree = '♣'
charForTile Grass = '·'

drawMap :: MapSegment -> ShadowMap -> Image
drawMap segment@((_, _, width, height), map) shadowMap = (vertCat . fmap rowImage) (range (0, height))
  where
    rowImage y = (horizCat . fmap (translateX 0 . imageAt)) row
      where row = range ((0, y), (width, y))
    imageAt coord | isVisible coord = char defAttr (charAt coord)
                  | otherwise = char defAttr ' '
    charAt coord = maybe '^' charForTile (lookupTile coord segment)
    isVisible coord = inRange (bounds shadowMap) coord && (shadowMap ! coord) == Visible