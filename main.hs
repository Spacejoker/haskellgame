import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as SDLi

import Data.Word
import Data.Tiled

import Model
import Animation
import Draw
import Event
import Logics
import MapLoader

import System.Random

-- idea: floor changes coloer semi-predictable, color determines tougness of battle

main = do
  SDL.init [InitEverything]
  setVideoMode 800 600 32 []
  TTF.init

  setCaption "RPG" "RPG" 

  --enableKeyRepeat 500 30

  tiledMap <- Data.Tiled.loadMapFile "map.tmx"
  tileSurface <- SDLi.load (iSource $ head $ tsImages $ head $ mapTilesets tiledMap)

  rng <- getStdGen

  fnt <- openFont "font.ttf" 30
  sheet <- SDLi.load "playerWalkDown.png"
  bg <- SDLi.load "menubg.bmp"
  fightbg <- SDLi.load "fight.png"
  menumarker <- SDLi.load "menumarker.png"
  menubg <- SDLi.load "menubg.png"
  enemyfire <- SDLi.load "enemyfire.png"
  explosion <- SDLi.load "explode.png"
  hitSprite <- SDLi.load "hit.png"
  
  t0 <- getTicks 
  
  let player = Player Down Stop (Position 300 300) (Animation sheet 26 4 250 t0 0 (Position 0 0) Nothing)
  let gx = Graphics tileSurface fightbg menumarker menubg enemyfire explosion sheet hitSprite
  let menu = Menu "Fight" 0 ["Attack", "Run"] (Position 0 340)
  let gs = (GameState True [] t0 player tiledMap (Position 32 32) Model.Walking rng 0 gx menu fnt [] [])
  let gs' = setUpNextFight gs ( fromIntegral (t0+1000) )

  gameLoop gs' t0

gameLoop :: GameState -> Word32 -> IO ()
gameLoop gs lastTick = do

  events <- getEvents pollEvent []
  t <- getTicks

  gs' <- updateGamestate (gameMode gs) (handleEvents events gs) t (t - lastTick)

  drawGamestate gs'

  if gameActive gs'
    then gameLoop gs' t
    else return ()

