module Logics where

import Graphics.UI.SDL as SDL

import Data.Word
import Data.Tiled

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

checkForFight :: GameState -> Word32 -> Mode
checkForFight gs t
  | fromIntegral t > nextFight gs = Model.Fight
  | otherwise = Model.Walking

--update both logics and graphics, in that order
updateGamestate :: GameState -> Word32 -> Word32 -> GameState
updateGamestate gs t dt
  -- Handle logic depending on gamestate, could need refactoring down the road
  | gameMode gs == Model.Walking = gs { animations = animations', player = player', cameraPos = cameraPos', gameMode = gameMode'}
  | gameMode gs == Model.AfterFight = (setUpNextFight gs ( fromIntegral (t+1000) )) { gameMode = Model.Walking }
  | otherwise = gs
    where animations' = updateAnimations (animations gs) t
          player' = updatePlayer (player gs) t dt
          cameraPos' = updateCamera (cameraPos gs) (playerPos $ player gs) (fromIntegral $ mapWidth $ currentMap gs) (fromIntegral $ mapHeight $ currentMap gs)
          gameMode' = checkForFight gs t

activateMenuOption :: GameState -> GameState
activateMenuOption gs
  | otherwise = gs
