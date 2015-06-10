{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module UI (play) where

import Graphics.Vty
import RoguePrelude
import Types
import Logic
import System.Random (getStdGen)
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Random (evalRandIO, evalRandT)
import FOV (Visibility(..), ShadowTopo)
import Control.Lens

data Action = ActionCommand Command
            | ActionMenu MenuPage
            | ActionQuit

data MenuPage = MenuInventory

data GameState = GameState { _gameStateMenuPage :: Maybe MenuPage
                           , _gameStateWorld :: World
                           }
$(makeFields ''GameState)

think :: memtype -> Mob -> Topo -> Thoughtful memtype a -> IO a
think mem mob topo brain = do
  gen <- getStdGen
  let readerPart = evalStateT brain mem
      randomPart = runReaderT readerPart (mob, topo)
  evalRandT randomPart gen

step :: ReaderT Vty (MaybeT (StateT GameState IO)) ()
step = do
  vty <- ask
  w <- view world <$> get
  liftIO (updateDisplay vty w)
  maybeAction <- parseEvent <$> liftIO (nextEvent vty)
  maybe (pure ()) (lift . interpretAction) maybeAction

interpretAction :: Action -> MaybeT (StateT GameState IO) ()
interpretAction ActionQuit = mzero
interpretAction (ActionMenu page) = menuPage .= Just page
interpretAction (ActionCommand c) = do
  w <- use world
  when (canPerformCommand (w ^. player) c w) $ do
    let brains = survey (w ^. mobs)

    worldSteppers <- for brains $ \(id, mob, brain) -> do
      command <- liftIO (think () mob (w ^. topo) brain)
      pure (performMobCommandIfPossible (id, command))

    world %= performCommand c
    world %= runAll worldSteppers
    world %=! evalRandIO . tick

infix 4 %=!

(%=!) :: (MonadIO m, MonadState t m) => Lens' t a -> (a -> IO a) -> m ()
lens %=! f = do
  s <- use lens
  s' <- liftIO (f s)
  lens .= s'

runAll :: [(a -> a)] -> a -> a
runAll fns state = foldr ($) state fns

play :: Vty -> World -> IO ()
play vty w = evalStateT (runMaybeT_ (runReaderT (forever step) vty)) gameState
  where gameState = GameState Nothing w

runMaybeT_ :: Monad m => MaybeT m a -> m ()
runMaybeT_ m = runMaybeT m $> ()

parseEvent :: Event -> Maybe Action
parseEvent (EvKey KUp [])    = c (Move North)
parseEvent (EvKey KDown [])  = c (Move South)
parseEvent (EvKey KRight []) = c (Move East)
parseEvent (EvKey KLeft [])  = c (Move West)
parseEvent (EvKey (KChar 'e') [])  = Just (ActionMenu MenuInventory)
parseEvent (EvKey (KChar 'q') []) = Just ActionQuit
parseEvent _ = Nothing

c :: Command -> Maybe Action
c = Just . ActionCommand

drawMob :: Mob -> Image
drawMob _ = char defAttr 'Z'

drawPlayer :: Player -> Image
drawPlayer _ = char defAttr '@'

positionImage :: HasLocation a Coord => Rect -> a -> Image -> Maybe Image
positionImage rect thing image
  | containsCoord rect coord = Just (translate x y image)
  | otherwise = Nothing
  where
    coord = thing ^. location
    (x, y) = globalToLocal rect coord

checkTopo :: HasLocation a Coord => ShadowTopo -> a -> Image -> Maybe Image
checkTopo shadowTopo thing image
  | (shadowTopo ! coord) == Visible = Just image
  | otherwise = Nothing
  where coord = thing ^. location

positionedThing :: HasLocation a Coord => Rect -> ShadowTopo -> (a -> Image) -> a -> Maybe Image
positionedThing rect shadowTopo draw thing = do
  positioned <- positionImage rect thing (draw thing)
  checkTopo shadowTopo thing positioned

drawWorld :: Rect -> World -> [Image]
drawWorld rect@(left, top, width, height) world = catMaybes (playerImage:mobImages) <> [mapImage]
  where
    playerImage = positionedThing rect globalShadowTopo drawPlayer (world ^. player)
    mobImages = positionedThing rect globalShadowTopo drawMob <$> (world ^.. mobs.traverse._2)
    mapImage = drawTopo localShadowTopo (rect, worldTopo)
    localShadowTopo = translateTopo (-left, -top) globalShadowTopo
    globalShadowTopo = makeShadowTopo (world ^. player) worldTopo
    worldTopo = world ^. topo

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

drawTopo :: ShadowTopo -> TopoSegment -> Image
drawTopo shadowTopo segment@((_, _, width, height), topo) = (vertCat . fmap rowImage) (range (0, height))
  where
    rowImage y = (horizCat . fmap (translateX 0 . imageAt)) row
      where row = range ((0, y), (width, y))
    imageAt coord | isVisible coord = char defAttr (charAt coord)
                  | otherwise = char defAttr ' '
    charAt coord = maybe '^' charForTile (lookupTile coord segment)
    isVisible coord = inRange (bounds shadowTopo) coord && (shadowTopo ! coord) == Visible
