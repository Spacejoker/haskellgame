module Animation where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Data.Word
import Data.Tiled
import Data.Maybe
import qualified Data.Map as Map

import Model

drawTile :: (Int, Int) -> Int -> Surface -> Surface -> Position -> IO ()
drawTile (x, y) id s tileSurface cameraPos = do
  
  let tiley = quot id 12
  let tilex = id `mod` 12
  let screenx = x*32 - (floor $ xVal cameraPos)
  let screeny = y*32 - (floor $ yVal cameraPos)

  blitSurface tileSurface (Just (Rect (tilex*32) (tiley*32) 32 32)) s (Just (Rect screenx screeny 32 32))
  return ()

drawTiles :: [(Int, Int)] ->  Map.Map (Int, Int) Tile -> Surface -> Surface -> Position -> IO ()
drawTiles [] _ _ _ _ = return ()
drawTiles ((x, y):xs) m s tileSurface cameraPos = do
  let t = Map.lookup (x, y) m
  if isJust t
    then drawTile (x, y) ((fromIntegral $ tileGid $ fromJust t)-1) s tileSurface cameraPos
    else return ()
  drawTiles xs m s tileSurface cameraPos

drawMap :: TiledMap -> Surface -> Surface -> Position -> IO ()
drawMap m s tileSurface cameraPos = do
  let l0 = head $ mapLayers m
  let tileData = layerData l0
  let fromTileX = floor $ (xVal cameraPos)/32.0
  let fromTileY = floor $ (yVal cameraPos)/32.0
  let coords = [(x, y) | x <- [fromTileX..(fromTileX + 25)], y <- [fromTileY..(fromTileY + 19)]]
  drawTiles coords tileData s tileSurface cameraPos
  return ()

drawWalkingMode :: GameState -> IO ()
drawWalkingMode gs = do
  s <- getVideoSurface
  drawMap (currentMap gs) s (tileSurface $ gx gs) (cameraPos gs)
  blitAnimations ((animation (player gs)) : animations gs) s (cameraPos gs)
  SDL.flip s

drawLabels :: [String] -> Position -> Surface -> Font -> IO()
drawLabels [] _ _ _ = return ()
drawLabels (x:xs) pos s fnt = do
  title <- renderTextSolid fnt x (Color 20 0 0)
  blitSurface title Nothing s (Just (Rect (floor $ xVal pos) (floor $ yVal pos) 200 200))
  drawLabels xs (Position ((xVal pos)) ((yVal pos) + 30)) s fnt

drawMenu :: GameState -> Surface -> IO ()
drawMenu gs s = do
  let menu' = menu gs
  let pos' = menuPos menu'
  let gx' = gx gs
  blitSurface (menubg gx') Nothing s (Just (Rect (floor $ xVal pos') (floor $ yVal pos') 20 20 ))
  blitSurface (menumarker gx') Nothing s (Just (Rect (floor $ xVal pos') ((floor $ yVal pos') + 10 +(choice menu') *30) 20 20 ))
  drawLabels (labels menu') (Position (((xVal pos') +10.0)) (((yVal pos') + 10.0))) s (fnt gs)
  
  return ()
  
drawFight :: GameState -> IO ()
drawFight gs = do
  s <- getVideoSurface
  blitSurface (fightbg $ gx gs) Nothing s Nothing
  blitSurface (enemyfire $ gx gs) Nothing s (Just (Rect 200 100 0 0))
  drawMenu gs s
  SDL.flip s

drawGamestate :: GameState -> IO ()
drawGamestate gs
  | gameMode gs == Model.Walking = drawWalkingMode gs
  | gameMode gs == Model.Fight = drawFight gs

blitAnimations :: [Animation] -> Surface -> Position -> IO()
blitAnimations [] _ _ =  return ()
blitAnimations (x:xs) s camera = do
  let startx = (width x) * (currentImage x)
  let xpos = xVal $ animPos x
  let ypos = yVal $ animPos x
  blitSurface (sheet x) (Just (Rect startx 0 ((width x)) 33)) s (Just (Rect (floor ( xpos - (xVal camera))) (floor (ypos - (yVal camera))) 1000 1000))
  blitAnimations xs s camera

nextFrame :: Animation -> Word32 -> Animation
nextFrame x t = x {lastSwitchTime = (fromIntegral t), currentImage = ((currentImage x)+1) `mod` frameCount x}

updateAnimations :: [Animation] -> Word32 -> [Animation]
updateAnimations [] _ = []
updateAnimations (x:xs) t
  | t - (lastSwitchTime x) > (fromIntegral $ imageTime x)  = ( nextFrame x t : updateAnimations xs t)
  | otherwise = (x : updateAnimations xs t)

