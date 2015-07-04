{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module UI (play) where

import Graphics.Vty
import Data.Map (Map)
import RoguePrelude
import Types
import Logic
import System.Random
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Random
import FOV (Visibility(..), ShadowTopo)
import Control.Lens

data Action = ActionCommand Command
            | ActionMenu MenuPage
            | ActionQuit

data MetaCommand = MetaQuit | MetaSave | MetaLoad

data MenuPage = MenuInventory

data GameState = GameState { _gameStateMenuPage :: Maybe MenuPage }
$(makeFields ''GameState)

modifio :: (MonadIO m, MonadState a m) => (a -> IO a) -> m ()
modifio f = do
  s <- get
  s' <- liftIO (f s)
  put s'

play :: Vty -> World -> IO ()
play vty w = evalStateT (runMaybeT_ (runReaderT (forever step) vty)) w

step :: ReaderT Vty (MaybeT (StateT World IO)) ()
step = do
  vty <- ask
  w <- get
  todoNext <- (liftIO . runMaybeT) (runEitherT (runReaderT (nextCommand w) vty))
  case todoNext of
    Nothing -> modifio (evalRandIO . tick)
    Just (Left MetaQuit) -> mzero
    Just (Right command) -> modify (performCommandIfPossible command)

nextCommand :: World -> ReaderT Vty (EitherT MetaCommand (MaybeT IO)) (Id, Command)
nextCommand w = do
  (id, actor) <- (lift . lift . liftMaybe) (nextActor w)
  vty <- ask
  command <- (lift . massage) (think vty w actor)
  pure (id, command)

massage :: EitherT a IO b -> EitherT a (MaybeT IO) b
massage = hoistEither <=< liftIO . runEitherT

nextActor :: World -> Maybe (Id, Actor)
nextActor w = ifind canAct (w ^. actors)
  where canAct _ actor = actor ^. nextTurn == w ^. turn

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe (Just x) = pure x
liftMaybe Nothing = mzero

zombieBrain :: MobBrain
zombieBrain = Move <$> randomDirection

playerBrain :: PlayerBrain
playerBrain = do
  (vty, w) <- ask
  liftIO (updateDisplay vty w)
  maybeAction <- parseEvent <$> liftIO (nextEvent vty)
  case maybeAction of
    Nothing -> playerBrain
    Just a -> do
      maybeCommand <- (lift . interpretAction) a
      maybe playerBrain pure maybeCommand

interpretAction :: Action -> StateT GameState (EitherT MetaCommand IO) (Maybe Command)
interpretAction ActionQuit = lift (left MetaQuit)
interpretAction (ActionMenu page) = (menuPage .= Just page) $> Nothing
interpretAction (ActionCommand c) = pure (Just c)

type PlayerBrain = ReaderT (Vty, World) (StateT GameState (EitherT MetaCommand IO)) Command
type MobBrain = ReaderT World (StateT Coord (Rand StdGen)) Command
data Brain = Player PlayerBrain | Mob MobBrain

think :: Vty -> World -> Actor -> EitherT MetaCommand IO Command
think vty world (view memory -> Usering) = do
  let brain' = runReaderT brain (vty, world)
  evalStateT brain' undefined
  where brain = playerBrain
think vty world (view memory -> Wandering) = do
  let brain' = runReaderT brain world
  let brain'' = evalStateT brain' undefined
  liftIO (evalRandIO brain'')
  where brain = zombieBrain

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

drawActor :: Actor -> Image
drawActor actor = char defAttr (charFor (actor ^. species))

charFor Zombie = 'Z'
charFor Human = '@'

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
drawWorld rect@(left, top, width, height) world = catMaybes actorImages <> [mapImage]
  where
    actorImages = positionedThing rect globalShadowTopo drawActor <$> (world ^. actors.to toList)
    mapImage = drawTopo localShadowTopo (rect, worldTopo)
    localShadowTopo = translateTopo (-left, -top) globalShadowTopo
    globalShadowTopo = makeShadowTopo (world ^. player) worldTopo
    worldTopo = world ^. topo

messageLog :: Actor -> Size -> Image
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
    minutes = ticks `div` 60
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

actorInfo :: Actor -> Size -> Image
actorInfo actor (width, height) = vertCat $ fmap (string defAttr)
  [ "Hunger: " <> (actor ^. hungerString)
  , "Fatigue: " <> (actor ^. fatigueString)
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
    makeMenu = stack [worldInfo world, actorInfo you, messageLog you]

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
