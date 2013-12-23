module DrawFight where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

import Data.Word
import Data.Tiled
import Data.Maybe
import qualified Data.Map as Map

import Model
import Animation

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
 
showHp :: [Enemy] -> Surface -> Font -> IO (Bool)
showHp [] _ _ = return (True)
showHp (x:xs) s fnt = do
  let str = "HP"
  title <- renderTextSolid fnt str (Color 20 0 0)
  let pos = animPos $ curAnimation x
  putStrLn $ show $ xVal pos
  blitSurface title Nothing s (Just (Rect (floor $ xVal pos) (floor $ (yVal pos - 20)) 200 200))
  
 
drawFight :: GameState -> IO ()
drawFight gs = do
  s <- getVideoSurface
  blitSurface (fightbg $ gx gs) Nothing s Nothing

  --blitSurface (enemyfire $ gx gs) Nothing s (Just (Rect 200 100 0 0))
  
  showHp (enemies gs) s (fnt gs)

  blitAnimations (map curAnimation (enemies gs)) s (Position 0 0)

  blitAnimations (animations gs) s (Position 0 0)
  drawMenu gs s
  SDL.flip s
