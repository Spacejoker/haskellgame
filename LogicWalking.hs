module LogicWalking where

import Model
import Data.Word
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

setupFight :: GameState -> Word32 -> GameState
setupFight gs t = gs { enemies = enemies' , gameMode = Model.Fight} -- create enemies here
  where enemies' = genEnemies gs

genEnemies :: GameState ->  [Enemy]
genEnemies gs = [Enemy "Rat 1" Model.Rat 10 10 1 1 1 animation (Position 10 10)]
  where animation = Animation (ratSprite $ gx gs) 80 80 2 250 (t gs) 0 (Position 100 100) Nothing

checkForFight :: GameState -> Word32 -> GameState
checkForFight gs t = gs
  -- | fromIntegral t > nextFight gs = setupFight gs t
  -- | otherwise = gs

