module DrawWorldMap where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Data.Word
import Data.Tiled
import Data.Maybe
import qualified Data.Map as Map

import Model
import Animation

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
