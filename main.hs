import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Data.Word

import Model
import Animation
import Event
import Logics

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

gameLoop :: Surface -> GameState -> Word32 -> IO ()
gameLoop image gs lastTick = do

  events <- getEvents pollEvent []
  t <- getTicks
  let gs' = updateGamestate (handleEvents events gs) t (t - lastTick)

  drawGamestate gs'

  if gameActive gs'
    then gameLoop image gs' t
    else return ()

