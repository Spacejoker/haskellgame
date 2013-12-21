module Logics where

import Graphics.UI.SDL as SDL

import Data.Word
import Data.Tiled

import Model
import Animation

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

--update both logics and graphics, in that order
updateGamestate :: GameState -> Word32 -> Word32 -> GameState
updateGamestate gs t dt = gs { animations = animations', player = player', cameraPos = cameraPos' }
  where animations' = updateAnimations (animations gs) t
        player' = updatePlayer (player gs) t dt
        cameraPos' = updateCamera (cameraPos gs) (playerPos $ player gs) (fromIntegral $ mapWidth $ currentMap gs) (fromIntegral $ mapHeight $ currentMap gs)

