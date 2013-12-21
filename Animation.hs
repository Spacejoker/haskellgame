module Animation where

import Graphics.UI.SDL as SDL

import Data.Word
import Data.Tiled
import Data.Maybe
import qualified Data.Map as Map

import Model

drawTile :: (Int, Int) -> Int -> Surface -> Surface -> IO ()
drawTile (x, y) id s tileSurface = do
  
  let tiley = quot id 12
  let tilex = id `mod` 12
  let screenx = x*32
  let screeny = y*32
  blitSurface tileSurface (Just (Rect (tilex*32) (tiley*32) 32 32)) s (Just (Rect screenx screeny 32 32))
  return ()

drawTiles :: [(Int, Int)] ->  Map.Map (Int, Int) Tile -> Surface -> Surface -> IO ()
drawTiles [] _ _ _ = return ()
drawTiles ((x, y):xs) m s tileSurface = do
  --tileSurface = 
  let t = Map.lookup (x, y) m
  if isJust t
    then drawTile (x, y) ((fromIntegral $ tileGid $ fromJust t)-1) s tileSurface
    else return ()
  drawTiles xs m s tileSurface

drawMap :: TiledMap -> Surface -> Surface -> IO ()
drawMap m s tileSurface = do
  let l0 = head $ mapLayers m
  let tileData = layerData l0
  let coords = [(x, y) | x <- [0..20], y <- [0..20]]
  drawTiles coords tileData s tileSurface
  --putStrLn $ show $ Map.lookup (0, 0) d--(layerData l0 (0 0))
  --blitSurface (layerImage l0) Nothing s Nothing
  return ()

drawGamestate :: GameState -> IO ()
drawGamestate gs = do
  s <- getVideoSurface
  drawMap (currentMap gs) s (tileSurface gs)
  blitAnimations ((animation (player gs)) : animations gs) s
  SDL.flip s

blitAnimations :: [Animation] -> Surface -> IO()
blitAnimations [] _ =  return ()
blitAnimations (x:xs) s = do
  let startx = (width x) * (currentImage x)
  let xpos = xVal $ animPos x
  let ypos = yVal $ animPos x
  blitSurface (sheet x) (Just (Rect startx 0 ((width x)) 33)) s (Just (Rect (floor xpos) (floor ypos) 1000 1000))
  blitAnimations xs s

nextFrame :: Animation -> Word32 -> Animation
nextFrame x t = x {lastSwitchTime = (fromIntegral t), currentImage = ((currentImage x)+1) `mod` frameCount x}

updateAnimations :: [Animation] -> Word32 -> [Animation]
updateAnimations [] _ = []
updateAnimations (x:xs) t
  | t - (lastSwitchTime x) > (fromIntegral $ imageTime x)  = ( nextFrame x t : updateAnimations xs t)
  | otherwise = (x : updateAnimations xs t)

