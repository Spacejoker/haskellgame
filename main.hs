import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Data.Word

import Model
import Animation
import Event

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []
  TTF.init

  setCaption "RPG" "RPG" 

  enableKeyRepeat 500 30

  fnt <- openFont "font.ttf" 30
  sheet <- SDLi.load "playerWalkDown.png"
  bg <- SDLi.load "menubg.bmp"
  
  t0 <- getTicks 
  let player = Player Down Stop (Position 20 20) (Animation sheet 26 4 250 t0 0 (Position 0 0))
  gameLoop bg (GameState True [] t0 player) t0

nextPlayerPos :: Player -> Word32 -> Position
nextPlayerPos player dt
  | speed player == Slow  && moveDirection player == Model.Right = Position (x0 + slowSpeed*(fromIntegral dt)) y0
  | speed player == Slow  && moveDirection player == Model.Left = Position (x0 - slowSpeed*(fromIntegral dt)) y0
  | speed player == Slow  && moveDirection player == Model.Up = Position x0 (y0 - slowSpeed*(fromIntegral dt))
  | speed player == Slow  && moveDirection player == Model.Down = Position x0 (y0 + slowSpeed*(fromIntegral dt))
  | otherwise = playerPos player
    where x0 = xVal $ playerPos player
          y0 = yVal $ playerPos player
          slowSpeed = 0.14

updatePlayer :: Player -> Word32 ->  Player
updatePlayer player t = player { animation = animation', playerPos = playerPos' }
  where animation' = (head  (updateAnimations  [animation player] t)) { animPos = playerPos player}
        playerPos' = nextPlayerPos player t

--update both logics and graphics, in that order
updateGamestate :: GameState -> Word32 -> GameState
updateGamestate gs dt = gs { animations = animations', player = player' }
  where animations' = updateAnimations (animations gs) dt
        player' = updatePlayer (player gs) dt

gameLoop :: Surface -> GameState -> Word32 -> IO ()
gameLoop image gs lastTick = do

  events <- getEvents pollEvent []
  let gs' = handleEvents events gs
  t <- getTicks
  let gs'' = updateGamestate gs' (t - lastTick)
  drawGamestate gs''

  if gameActive gs''
    then gameLoop image gs'' t
    else return ()

