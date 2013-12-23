module Logics where

import Graphics.UI.SDL as SDL

import Data.Word
import Data.Tiled
import Data.Maybe

import Model
import Animation

import System.Random

setUpNextFight :: GameState -> Int -> GameState
setUpNextFight gs t = gs {rng = rng', nextFight = nextFight'}
  where (val, rng') = next $ rng gs
        nextFight' = val `mod` 1000 + t + 2000

nextPlayerPos :: Player -> Word32 -> Position
nextPlayerPos player dt
  | speed player == Slow  && moveDirection player == Model.Right = Position (x0 + slowSpeed*(fromIntegral dt)) y0
  | speed player == Slow  && moveDirection player == Model.Left = Position (x0 - slowSpeed*(fromIntegral dt)) y0
  | speed player == Slow  && moveDirection player == Model.Up = Position x0 (y0 - slowSpeed*(fromIntegral dt))
  | speed player == Slow  && moveDirection player == Model.Down = Position x0 (y0 + slowSpeed*(fromIntegral dt))
  | otherwise = playerPos player
    where x0 = xVal $ playerPos player
          y0 = yVal $ playerPos player
          slowSpeed = 0.2

updatePlayer :: Player -> Word32 -> Word32 ->  Player
updatePlayer player t dt = player { animation = animation', playerPos = playerPos' }
  where animation' = (head  (updateAnimations  [animation player] t)) { animPos = playerPos player}
        playerPos' = nextPlayerPos player dt

posDiff :: Float -> Float -> Float -> Float -> Float
posDiff minval maxval a b
  | (b-a) < minval = b - minval
  | (b-a) > maxval = b - maxval
  | otherwise = a

updateCamera :: Position -> Position -> Float -> Float -> Position
updateCamera cameraPos playerPos xdim ydim = Position (min (32*xdim-800)  (max 0 xpos)) (min (32*ydim-600) (max 0 ypos))
  where xpos = posDiff 200 600 (xVal cameraPos) (xVal playerPos)
        ypos = posDiff 200 400 (yVal cameraPos) (yVal playerPos)

setupFight :: GameState -> GameState
setupFight gs = gs { enemies = [] , gameMode = Model.Fight} -- create enemies here

checkForFight :: GameState -> Word32 -> GameState
checkForFight gs t
  | fromIntegral t > nextFight gs = setupFight gs
  | otherwise = gs

performFightActions :: GameState -> [String] -> Word32 -> IO(GameState)
performFightActions gs [] _ = return (gs)
performFightActions gs ("Attack":xs) t = do
  putStrLn "Attacking"
  let hitAnim = Animation (hitSprite $ gx gs) 70 3 100 t 0 (Position 0 0) (Just (t + 300))
  return (gs {animations = (hitAnim : (animations gs))})
  

updateGamestate :: Model.Mode -> GameState -> Word32 -> Word32 -> IO(GameState)
updateGamestate Model.Walking gs t dt = do
  let animations' = updateAnimations (animations gs) t
  let player' = updatePlayer (player gs) t dt
  let cameraPos' = updateCamera (cameraPos gs) (playerPos $ player gs) (fromIntegral $ mapWidth $ currentMap gs) (fromIntegral $ mapHeight $ currentMap gs)
  let gs' = checkForFight gs t
  return (gs' { animations = animations', player = player', cameraPos = cameraPos'})

updateGamestate Model.AfterFight gs t dt = do
  let gs' = (setUpNextFight gs ( fromIntegral (t+1000) )) { gameMode = Model.Walking, actions = [] }
  return (gs')

updateGamestate Model.Fight gs t dt = do
  gs' <- performFightActions gs (actions gs) t
  let animations' = updateAnimations (animations gs') t
  return (gs' {actions = [], animations = filter (\x -> fromJust ( expire x ) > t) (animations')})

activateMenuOption :: GameState -> String -> Int -> GameState
activateMenuOption gs "Fight" 0 = gs { actions = actions'}
  where actions' = ("Attack" : actions gs)
activateMenuOption gs "Fight" 1 = gs {gameMode = Model.AfterFight }
