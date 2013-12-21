import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Data.Word
import Data.Tiled

import Model
import Animation
import Event
import Logics
import MapLoader

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []
  TTF.init

  setCaption "RPG" "RPG" 

  --enableKeyRepeat 500 30

  tiledMap <- Data.Tiled.loadMapFile "map.tmx"
  tileSurface <- SDLi.load (iSource $ head $ tsImages $ head $ mapTilesets tiledMap)
  --let image =  head $ mapTilesets m

  fnt <- openFont "font.ttf" 30
  sheet <- SDLi.load "playerWalkDown.png"
  bg <- SDLi.load "menubg.bmp"
  
  t0 <- getTicks 

  let player = Player Down Stop (Position 300 300) (Animation sheet 26 4 250 t0 0 (Position 0 0))
  gameLoop (GameState True [] t0 player tiledMap tileSurface (Position 32 32)) t0

gameLoop :: GameState -> Word32 -> IO ()
gameLoop gs lastTick = do

  events <- getEvents pollEvent []
  t <- getTicks
  let gs' = updateGamestate (handleEvents events gs) t (t - lastTick)

  drawGamestate gs'

  if gameActive gs'
    then gameLoop gs' t
    else return ()

