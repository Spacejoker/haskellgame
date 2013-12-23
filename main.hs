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

  fnt <- openFont "LiberationMono-Bold.ttf" 20
  sheet <- SDLi.load "image/playerWalkDown.png"
  fightbg <- SDLi.load "image/fight.png"
  menumarker <- SDLi.load "image/menumarker.png"
  menubg <- SDLi.load "image/menubg.png"
  enemyfire <- SDLi.load "image/enemyfire.png"
  explosion <- SDLi.load "image/explode.png"
  hitSprite <- SDLi.load "image/hit.png"
  ratSprite <- SDLi.load "image/rat.png"
  
  t0 <- getTicks 
  
  let player = Player Down Stop (Position 300 300) (Animation sheet 26 70 4 250 t0 0 (Position 0 0) Nothing)
  let gx = Graphics tileSurface fightbg menubg menumarker enemyfire explosion sheet hitSprite ratSprite
  let menu = Menu "Fight" 0 ["Attack", "Run"] (Position 0 340)
  let gs = (GameState True [] t0 player tiledMap (Position 32 32) Model.Walking rng 0 gx menu fnt [] [] t0)
  let gs' = setUpNextFight gs ( fromIntegral (t0+1000) )

  gameLoop gs' t0

gameLoop :: GameState -> Word32 -> IO ()
gameLoop gs lastTick = do

  events <- getEvents pollEvent []
  t <- getTicks

  gs' <- updateGamestate (gameMode gs) (handleEvents events gs) t (t - lastTick)

  drawGamestate gs'

  if gameActive gs'
    then gameLoop gs'{t = t} t
    else return ()

